package web

import common.types.SessionId
import data.ImplTestsRepo
import error.{AppError, ConfigError, DatabaseError, ErrorResponse, NotFoundError, ValidationError, WebServiceError} // Added AppError subtypes and ErrorResponse
import runner.{TestRunner, TestRunnerImpl}
import tmodel.{RespTest, RespTestModel, Session, TestModel, TestsToRun}
import zio.http._
import zio.http.template.Html
import zio.json.{DecoderOps, EncoderOps}
import zio.{Scope, ZIO, ZLayer}

import java.io.IOException
import scala.io._

object WebUiApp {

  def getMainPage: ZIO[Any, IOException, Response] = ZIO.scoped {
    for {
      _ <- ZIO.logInfo("getMainPage")
      inputStream <- ZIO.attemptBlockingIO(
        Option(getClass.getResourceAsStream("/index.html"))
      ).mapError(e => WebServiceError(s"Failed to load index.html: ${e.getMessage}", Some(e))) // Mapped to WebServiceError
        .someOrFail(NotFoundError("index.html not found")) // Mapped to NotFoundError
      source <- ZIO.fromAutoCloseable(
        ZIO.succeed(scala.io.Source.fromInputStream(inputStream))
      )
      content <- ZIO.attemptBlockingIO(source.mkString)
      resp <- ZIO.succeed(Response.html(Html.raw(content)))
    } yield resp
  }


  import data.EncDeccheckTestRepoDataImplicits._
  def checkTestsRepo(sid: SessionId): ZIO[ImplTestsRepo, AppError, Response] = // Changed IOException to AppError
    for {
      tr <- ZIO.service[ImplTestsRepo]
      info <- tr.checkTestRepoData(sid).mapError(e => DatabaseError(s"Failed to check tests repo for SID $sid: ${e.getMessage}", Some(e))) // Mapped to DatabaseError
      resp <- ZIO.succeed(info match {
        case Some(i) => Response.json(i.toJson)
        // Consider if ResponseMessage should be replaced or adapted for non-error cases too. For now, it remains.
        case _ => Response.json(error.ResponseMessage("OK start tests").toJson)
      })
    } yield resp

  import tmodel.EncDecTestModelImplicits._

  def getTestInfo(sid: SessionId, testId: Int): ZIO[ImplTestsRepo, AppError, Response] = // Changed IOException to AppError
    for {
      _ <- ZIO.logInfo("getTestInfo ")
      tr <- ZIO.service[ImplTestsRepo]
      tests <- tr.testsList(sid).mapError(e => DatabaseError(s"Failed to get tests list for SID $sid: ${e.getMessage}", Some(e))) // Mapped to DatabaseError
      resp = tests match {
        case Some(testsList) => testsList.find(_.id == testId) match {
          case Some(thisTest) =>
            Response.html(thisTest.getTestAsHtml, Status.Ok)
          case None =>
            Response.json(ErrorResponse("NotFoundError", s"Test [$testId] not found in repo for SID $sid.").toJson).status(Status.NotFound) // Used ErrorResponse
        }
        case None =>
          Response.json(ErrorResponse("NotFoundError", s"No tests found for SID $sid.").toJson).status(Status.NotFound) // Used ErrorResponse
      }
    } yield resp


  import tmodel.EncDecRespTestModelImplicits._

  private def handleLoadedTestsProcessing(creationResult: Either[String, TestModel], testsRepo: ImplTestsRepo): ZIO[ImplTestsRepo, AppError, Response] =
    creationResult match {
      case Left(errorMsg) => 
        ZIO.logError(s"Error parsing TestModel JSON: $errorMsg") *>
          ZioResponseError(ValidationError(s"Invalid TestModel JSON: $errorMsg"))
      case Right(testModel) => 
        testsRepo.create(testModel).flatMap { sid =>
          ZIO.logInfo(s"Successfully created tests with SID = $sid") *>
            testsRepo.testsList(sid)
              .mapError(e => DatabaseError(s"Failed to list tests for SID $sid after creation: ${e.getMessage}", Some(e)))
              .map { optTests =>
                Response.json(RespTestModel(
                  Session(sid),
                  optTests.map { trp => trp.map { testInRepo =>
                    RespTest(testInRepo.id, s"[${testInRepo.id}] ${testInRepo.name}") 
                  }}
                ).toJson)
              }
        }.foldZIO(
          error   => ZIO.logError(s"Error processing created tests: ${error.message}") *> ZioResponseError(error),
          success => ZIO.succeed(success)
        )
    }

  def loadTests(req: Request): ZIO[ImplTestsRepo, AppError, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
      bodyAsStr <- req.body.asString.mapError(e => ValidationError(s"Error reading request body for loadTests: ${e.getMessage}"))
      parsedModel = bodyAsStr.fromJson[TestModel]
      resp <- handleLoadedTestsProcessing(parsedModel, tr)
    } yield resp

  private def startTestsLogic(testsToRun: TestsToRun): ZIO[ImplTestsRepo with TestRunner, AppError, Unit] = for {
    tr <- ZIO.service[ImplTestsRepo]
    _ <- ZIO.logInfo(s" testsToRun = ${testsToRun.sid} - ${testsToRun.ids}")
    _ <- ZIO.logInfo(s" Call disableAllTest for sid = ${testsToRun.sid}")
      .when(testsToRun.ids.getOrElse(List[Int]()).isEmpty)
    _ <- tr.disableAllTestAndClearExecRes(testsToRun.sid).mapError(e => DatabaseError(s"DB error disabling tests for SID ${testsToRun.sid}: ${e.getMessage}", Some(e))) // Mapped to DatabaseError
    _ <- ZIO.foreachDiscard(testsToRun.ids.getOrElse(List[Int]())) {
      testId => tr.enableTest(testsToRun.sid, testId).mapError(e => DatabaseError(s"DB error enabling test $testId for SID ${testsToRun.sid}: ${e.getMessage}", Some(e))) // Mapped to DatabaseError
    }
    testRunner <- ZIO.service[TestRunner]
    _ <- testRunner.run().mapError(e => WebServiceError(s"Test runner failed: ${e.getMessage}", Some(e))) // Mapped to WebServiceError
  } yield ()


  def ZioResponseError(error: AppError): ZIO[Any, Nothing, Response] = { // Changed signature
    val (status, errorType) = error match {
      case ConfigError(_) => (Status.InternalServerError, "ConfigError")
      case DatabaseError(_, _) => (Status.InternalServerError, "DatabaseError")
      case ValidationError(_) => (Status.BadRequest, "ValidationError")
      case NotFoundError(_) => (Status.NotFound, "NotFoundError")
      case WebServiceError(_, _) => (Status.InternalServerError, "WebServiceError")
      // case _ => (Status.InternalServerError, "UnknownError") // Should not happen with sealed trait
    }
    ZIO.succeed(Response.json(ErrorResponse(errorType, error.message).toJson).status(status))
  }

  /**
   * Start selected tests (array of id) from Tests set identified by sid.
  */
  
  private def handleStartTestsProcessing(parsedTestsToRun: Either[String, TestsToRun], testsRepo: ImplTestsRepo): ZIO[ImplTestsRepo, AppError, Response] =
    parsedTestsToRun match {
      case Left(errorMsg) =>
        ZIO.logError(s"Error parsing TestsToRun JSON: $errorMsg") *>
          ZioResponseError(ValidationError(s"Invalid TestsToRun JSON: $errorMsg"))
      case Right(testsToRun) =>
        startTestsLogic(testsToRun)
          .provide(ZLayer.succeed(testsRepo), TestRunnerImpl.layer, ZLayer.succeed(testsToRun.sid))
          .foldZIO(
            appError => ZIO.logError(s"Error starting tests execution: ${appError.message}") *> ZioResponseError(appError),
            _ => ZIO.succeed(Response.json(error.ResponseMessage("OK Tests Started").toJson)) // Consistent "OK" message
          )
    }

  def startTests(req: Request): ZIO[ImplTestsRepo, AppError, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
      bodyAsStr <- req.body.asString.mapError(e => ValidationError(s"Error reading request body for startTests: ${e.getMessage}"))
      parsedContents = bodyAsStr.fromJson[TestsToRun]
      resp <- handleStartTestsProcessing(parsedContents, tr)
    } yield resp

  /**
   * Add catchAll common part to effect.
  */
  private def catchCover[C](eff: ZIO[C, AppError, Response]): ZIO[C, Nothing, Response] = // Changed Exception to AppError
    eff.catchAll { appError =>
      ZIO.logError(s"Caught error: ${appError.getClass.getSimpleName} - ${appError.message}") *> ZioResponseError(appError) // Use new error handler
    }

  val app: Routes[ImplTestsRepo, Nothing] =
    Routes(
      Method.GET / "main" -> handler{getMainPage.catchAll{appError =>
        ZIO.logError(s"Error in getMainPage: ${appError.message}") *> ZioResponseError(appError) // Use new error handler
      }},
      Method.GET / "check" / string("sid") -> handler {
        (sid: String, _: Request) => catchCover(checkTestsRepo(sid))
      },
      Method.GET / "test_info" / string("sid") / int("testId") -> handler {
        (sid: String, testId: Int, _: Request) => catchCover(getTestInfo(sid, testId))
      },
      Method.POST / "load_test"  -> handler{(req: Request) => catchCover(loadTests(req))},
      Method.POST / "start_test" -> handler{(req: Request) => catchCover(startTests(req))}
    )

}

