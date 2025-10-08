package web

import common.types.SessionId
import data.ImplTestsRepo
import db.jdbcSessionImpl
import error.ResponseMessage
import runner.{TestRunner, TestRunnerImpl}
import tmodel.{RespTest, RespTestModel, Session, TestModel, TestsToRun}
import zio.http._
import zio.http.template.Html
import zio.json.{DecoderOps, EncoderOps}
import zio.{Scope, ZIO, ZLayer}

import java.io.IOException
import java.nio.file.{Files, StandardOpenOption}

object WebUiApp {

  def getMainPage: ZIO[Any, IOException, Response] = ZIO.scoped {
    for {
      _ <- ZIO.logInfo("getMainPage")
      inputStream <- ZIO.attemptBlockingIO(
        Option(getClass.getResourceAsStream("/index.html"))
      ).someOrFail(new IOException("Resource not found"))
      source <- ZIO.fromAutoCloseable(
        ZIO.succeed(scala.io.Source.fromInputStream(inputStream))
      )
      content <- ZIO.attemptBlockingIO(source.mkString)
      resp <- ZIO.succeed(Response.html(Html.raw(content)))
    } yield resp
  }

  def getRepoInfo: ZIO[ImplTestsRepo, Nothing, Response] =  for {
    tr <- ZIO.service[ImplTestsRepo]
    r <- tr.ref.get
  } yield Response.json(r.keys.toList.toJson)

  import data.EncDeccheckTestRepoDataImplicits._
  def checkTestsRepo(sid: SessionId): ZIO[ImplTestsRepo, IOException, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
      info <- tr.checkTestRepoData(sid)
      resp <- ZIO.succeed(info match {
        case Some(i) => Response.json(i.toJson)
        case _ => Response.json(ResponseMessage("OK start tests").toJson)
      })

    } yield resp

  import tmodel.EncDecTestModelImplicits._

  def getTestInfo(sid: SessionId, testId: Int): ZIO[ImplTestsRepo, IOException, Response] =
    for {
      _ <- ZIO.logInfo("getTestInfo ")
      tr <- ZIO.service[ImplTestsRepo]
      tests <- tr.testsList(sid)
      resp = (tests match {
        case Some(testsList) => testsList.find(_.id == testId) match {
          case Some(thisTest) =>
            Response.html(thisTest.getTestAsHtml, Status.Ok)
          case None => Response.json(ResponseMessage(s"Test [$testId] not found in repo.").toJson)
            .status(Status.BadRequest)
        }
        case None => Response.json(ResponseMessage(s"Test [$testId] not found in repo.").toJson)
          .status(Status.BadRequest)
      })
    } yield resp

  def removeSidFromRepo(sid: SessionId): ZIO[ImplTestsRepo, IOException, Response] =
    for {
      _ <- ZIO.logInfo(s"removeSidFromRepo for sid = $sid ")
      testRepo <- ZIO.service[ImplTestsRepo]
      _ <- testRepo.remove(sid)
      resp = Response.json(ResponseMessage(s"All tests and sid removed from repo, sid = $sid").toJson)
    } yield resp

  import tmodel.EncDecRespTestModelImplicits._
  def loadTests(req: Request): ZIO[ImplTestsRepo, Exception, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
      bodyAsStr <- req.body.asString.catchAll{
        case e: Exception => ZIO.logError(s"pgt-0 error parsing input file with tests : ${e.getMessage}").as("{}")
      }
      u <- ZIO.attempt(bodyAsStr.fromJson[TestModel]).either
      resp <- u match {
        case Left(exp_str) => ZIO.logError(s"Error in input json - ${exp_str.getMessage}") *>
          ZioResponseMsgBadRequest(exp_str.getMessage)
        case Right(testsWithMetaE) =>
          testsWithMetaE match {
            case Left(str) => ZioResponseMsgBadRequest(str)
            case Right(testsWithMeta) => tr.create(testsWithMeta).flatMap { sid =>
              ZIO.logInfo(s"SID = $sid") *>
                tr.testsList(sid).map {
                  optTests =>
                    Response.json(RespTestModel(
                      Session(sid),
                      optTests.map { trp => trp.map { testInRepo =>
                        RespTest(testInRepo.id, s"[${testInRepo.id}] ${testInRepo.name}") } }
                    ).toJson)
                }
            }.foldZIO(
              error   => ZIO.logError(s"pgt-3 error parsing input file with tests : ${error.getMessage}") *>
                ZioResponseMsgBadRequest(error.getMessage),
              success => ZIO.succeed(success)
            )
          }

      }
    } yield resp


  def loadTestsExcel(req: Request): ZIO[ImplTestsRepo, Exception, Response] = for {
    tr <- ZIO.service[ImplTestsRepo]
    _ <- ZIO.logInfo("GET EXCEL FILE WITH TESTS....................")
    form  <- req.body.asMultipartForm.mapError {
      case e: Exception => e
      case t            => new Exception(t)
    }
    resp  <- (form.get("file") match {
      case Some(FormField.Binary(_, data, contentType, _, filename)) =>
        ZIO.scoped {
          ZIO.attemptBlocking(Files.createTempFile("upload-", filename.getOrElse("uploaded") + ".xlsx")).flatMap { tmp =>
            ZIO.logInfo(s"Temp excel file is = $tmp  contentType=[$contentType]") *>
            ZIO.attemptBlocking {
              Files.write(tmp, data.toArray, StandardOpenOption.WRITE)
            } *> {
              // Вызов парсера, ожидающего Path
              for {
                tm <- ExcelParser.parse(tmp)
                ri <- tr.create(tm).flatMap {sid =>
                ZIO.logInfo (s"SID = $sid") *>
                tr.testsList (sid).map {
                optTests =>
                Response.json (RespTestModel (
                Session (sid),
                optTests.map {trp => trp.map {testInRepo =>
                RespTest (testInRepo.id, s"[${testInRepo.id}] ${testInRepo.name}")}}
              ).toJson)
              }
              }.foldZIO(
                error => ZIO.logError(s"pgt-3 error parsing input file with tests : ${error.getMessage}") *>
                           ZioResponseMsgBadRequest(error.getMessage),
                success => ZIO.succeed(success)
              )
              } yield ri

            }.ensuring(ZIO.attemptBlocking(Files.deleteIfExists(tmp)).ignore) // по желанию удалить
          }
        }
      case _ => ZioResponseMsgBadRequest("Missing 'file' multipart field")
    }).refineToOrDie[Exception]
    } yield resp

  private def startTestsLogic(testsToRun: TestsToRun): ZIO[Scope with ImplTestsRepo with TestRunner, Exception, Unit] = for {
    tr <- ZIO.service[ImplTestsRepo]
    _ <- ZIO.logInfo(s" testsToRun = ${testsToRun.sid} - ${testsToRun.ids}")
    _ <- ZIO.logInfo(s" Call disableAllTest for sid = ${testsToRun.sid}")
      .when(testsToRun.ids.getOrElse(List[Int]()).isEmpty)
    _ <- tr.disableAllTestAndClearExecRes(testsToRun.sid)
    _ <- ZIO.foreachDiscard(testsToRun.ids.getOrElse(List[Int]())) {
      testId => tr.enableTest(testsToRun.sid, testId)
    }
    testsSetOpt <- tr.lookup(testsToRun.sid)
    testRunner <- ZIO.service[TestRunner]
    _ <- testsSetOpt match {
      case Some(testsSet) => testRunner.run().provideSome[Scope](jdbcSessionImpl.layer, ZLayer.succeed(testsSet.meta))
      case None => ZIO.unit
    }
  } yield ()


  def ZioResponseMsgBadRequest(message: String): ZIO[Any,Nothing,Response] =
    ZIO.succeed(Response.json(ResponseMessage(message).toJson).status(Status.BadRequest))

  /**
   * Start selected tests (array of id) from Tests set identified by sid.
  */
  def startTests(req: Request): ZIO[Scope with ImplTestsRepo, Exception, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
      u <- req.body.asString.map(_.fromJson[TestsToRun])
        .catchAll{
          case e: Exception => ZIO.left(e.getMessage)
        }
      resp <- u match {
        case Left(exp_str) => ZioResponseMsgBadRequest(exp_str)
        case Right(testsToRun) =>
          startTestsLogic(testsToRun).provideSome[Scope](ZLayer.succeed(tr),TestRunnerImpl.layer, ZLayer.succeed(testsToRun.sid))
          .foldZIO(
            err => ZIO.logError("-- this point --") *> ZioResponseMsgBadRequest(err.getMessage),
            _ => ZIO.succeed(Response.json(ResponseMessage("OK").toJson))
          )
      }
    } yield resp

  /**
   * Add catchAll common part to effect.
  */
  private def catchCover[C](eff: ZIO[C, Exception, Response]): ZIO[C, Nothing, Response] =
    eff.catchAll{e: Exception =>
      ZIO.logError(e.getMessage) *> ZioResponseMsgBadRequest(e.getMessage)
    }

  val app: Routes[ImplTestsRepo, Nothing] =
    Routes(
      Method.GET / "main" -> handler{getMainPage.catchAll{e: Exception =>
        ZIO.logError(e.getMessage) *> ZioResponseMsgBadRequest(e.getMessage)
      }},
      Method.GET / "check" / string("sid") -> handler {
        (sid: String, _: Request) => catchCover(checkTestsRepo(sid))
      },
      Method.GET / "test_info" / string("sid") / int("testId") -> handler {
        (sid: String, testId: Int, _: Request) => catchCover(getTestInfo(sid, testId))
      },
      Method.POST / "load_test"  -> handler{(req: Request) => catchCover(loadTests(req))},
      Method.POST / "load_test_excel"  -> handler{(req: Request) => catchCover(loadTestsExcel(req))},
      //todo: new load_test_excel + loadTestsExcel
      Method.POST / "start_test" -> handler{
        (req: Request) =>
          ZIO.scoped {catchCover(startTests(req))}
      },
      Method.GET / "repo_info" -> handler{getRepoInfo},
      Method.GET / "remove_sid" / string("sid") -> handler{
        (sid: String, _: Request) => catchCover(removeSidFromRepo(sid))
      }
    )

}

