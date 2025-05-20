package runner

import common.{CallTimings, TestExecutionResult}
import common.types.SessionId
import data.ImplTestsRepo
import db.{jdbcSession, jdbcSessionImpl, pgSess}
import org.postgresql.jdbc.PgResultSet
import tmodel.{Test, TestsMeta, Affected_rows, Cursor, Dataset, Dml_sql, Func_inout_cursor, Integer_value, Select, Select_function}
import zio.metrics.{Metric, MetricLabel}
import zio.{UIO, ZIO, ZLayer}
import common.types._

import java.sql.{Connection, ResultSet}

  trait TestRunner {
    def run(): ZIO[Any, Exception, Unit]
  }

  case class TestRunnerImpl(tr: ImplTestsRepo, sid: SessionId) extends TestRunner {
  import java.sql.Types

  private def updateTestWithResult(test: Test): ZIO[Any, Exception, Unit] = for {
    _ <- tr.updateTestWithResults(sid, test.checkConditions)
  } yield ()

  /**
   * Goes through input ResultSet or PgResultSet and return columns with types and rows as List of Seq[String]
  */
  private def columnsRows[A <: ResultSet](rs: A): (Columns,ListRows) ={
    val columns: Columns = (1 to rs.getMetaData.getColumnCount)
      .map(cnum => (rs.getMetaData.getColumnName(cnum), rs.getMetaData.getColumnTypeName(cnum)))
    val resultsCur: Iterator[IndexedSeq[String]] = Iterator.continually(rs).takeWhile(_.next()).map {
      rs => columns.map(cname => rs.getString(cname._1))
    }
    val results: ListRows = Iterator.continually(resultsCur).takeWhile(itr => itr.hasNext).flatten.toList
    rs.close()
    (columns,results)
  }

  private def execCallUpdateTestInRepo(dbCall: UIO[TestExecutionResult], test: Test): ZIO[Any,Exception,Unit] = for {
    testResult <- dbCall
    _ <- updateTestWithResult(test.copy(
      isExecuted = true,
      testRes = testResult,
      countOfExecuted = test.countOfExecuted+1))
  } yield ()

  private def makeCommit(use: Option[Boolean], conn: Connection) =
    if (use.getOrElse(false))
      conn.commit()

  private def exec_func_inout_cursor(pgses: pgSess, testInRepo: Test): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val procCallText = s"{call ${testInRepo.call} }"
      val stmt = connection.prepareCall(procCallText);
      stmt.setNull(1, Types.OTHER)
      stmt.registerOutParameter(1, Types.OTHER)
      val tBegin = System.currentTimeMillis
      stmt.execute()
      val tExec = System.currentTimeMillis
      val pgrs = stmt.getObject(1).asInstanceOf[PgResultSet]
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      makeCommit(testInRepo.use_commit, connection)
      stmt.close()
      connection.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def exec_select_dataset(pgses: pgSess, testInRepo: Test): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.createStatement()
      val tBegin = System.currentTimeMillis
      val pgrs: ResultSet = stmt.executeQuery(testInRepo.call);
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      makeCommit(testInRepo.use_commit, connection)
      stmt.close()
      connection.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def exec_select_function_int(pgses: pgSess, testInRepo: Test): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.createStatement()
      val tBegin = System.currentTimeMillis
      val pgrs: ResultSet = stmt.executeQuery(testInRepo.call);
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      makeCommit(testInRepo.use_commit, connection)
      stmt.close()
      connection.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def exec_dml_sql(pgses: pgSess, testInRepo: Test): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.createStatement()
      val tBegin = System.currentTimeMillis
      val hasResultSet = stmt.execute(testInRepo.call)
      val rowsAffected: Int =
        if (hasResultSet) {
          0// Обрабатываем результат для SELECT, если в нем вызывается функция
        } else {
          stmt.getUpdateCount // Получаем количество изменённых строк для INSERT/UPDATE/DELETE
        }
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
        //val (cols: Columns, rows: ListRows) = (IndexedSeq[Column](),List[IndexedSeq[String]]())
        val cols: Columns = IndexedSeq[Column]()
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rowsAffected)
      }
      makeCommit(testInRepo.use_commit, connection)
      stmt.close()
      connection.close()
      res
    }
    execDbCallCatched = catchAllErrsWithCommit(execDbCall,connection)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def exec_select_function_cursor(pgses: pgSess, testInRepo: Test): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.prepareStatement(testInRepo.call)
      val tBegin = System.currentTimeMillis
      val rs: ResultSet = stmt.executeQuery()
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
       if (!rs.next())
          TestExecutionResult()
        val pgrs = rs.getObject(1).asInstanceOf[PgResultSet]
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      makeCommit(testInRepo.use_commit, connection)
      stmt.close()
      connection.close()
      res
  }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def catchAllErrs(eff: ZIO[Any, Throwable, TestExecutionResult]): UIO[TestExecutionResult] =
    eff
      .tapError(e => ZIO.logError(s"Expected failure: ${e.getMessage}"))
      .catchAll {
        e => ZIO.succeed(TestExecutionResult(e.getClass.getName, e.getMessage))
      }

    private def catchAllErrsWithCommit(eff: ZIO[Any, Throwable, TestExecutionResult],connection: Connection): UIO[TestExecutionResult] =
      eff
        .tapError(e => ZIO.logError(s"Expected failure: ${e.getMessage}"))
        .catchAll {  // Ловим ожидаемые ошибки (Throwable)
          e => //ZIO.logInfo(s"Error ${e.getMessage} ${e.getClass.getName}") *>
            ZIO.attemptBlocking {
              println("******* DEBUG COMMIT AND CLOSE CONNECTION ***********")
              connection.commit()
              connection.close()
            }.as(TestExecutionResult(e.getClass.getName, e.getMessage))
        }.orDie

  private def exec(testInRepo: Test): ZIO[TestsMeta with jdbcSession, Exception, Unit] = for {
    jdbc <- ZIO.service[jdbcSession]
    conn <- jdbc.pgConnection(testInRepo.id)
    _ <- ZIO.logInfo(s" ----> sid=[$sid] tests [${testInRepo.id}] isOpened Connection = ${!conn.sess.isClosed}")
    _ <- (testInRepo.call_type,testInRepo.ret_type) match {
      case (_: Select_function.type, _: Cursor.type) => exec_select_function_cursor(conn,testInRepo)
      case (_: Select_function.type, _: Integer_value.type) => exec_select_function_int(conn,testInRepo)
      case (_: Func_inout_cursor.type , _: Cursor.type) => exec_func_inout_cursor(conn,testInRepo)
      case (_: Select.type, _: Dataset.type) => exec_select_dataset(conn,testInRepo)
      case (_: Dml_sql.type, _: Affected_rows.type) => exec_dml_sql(conn,testInRepo)
      case _ => ZIO.unit
    }
  } yield ()

  def run(): ZIO[Any, Exception, Unit] = for {
      testsSetOpt <- tr.lookup(sid)
      _ <- testsSetOpt match {
        case Some(testsSet) =>
          ZIO.logInfo(s" Begin tests set execution for SID = $sid") *>
          ZIO.foreachDiscard(testsSet.optListTestInRepo.getOrElse(List[Test]()).filter(_.isEnabled == true)) {
            testId: Test =>
              exec(testId).provide(ZLayer.succeed(testsSet.meta), jdbcSessionImpl.layer)
          }
        case None => ZIO.unit
      }
    } yield ()
  }

  object TestRunnerImpl {
    val layer: ZLayer[ImplTestsRepo with SessionId, Exception, TestRunner] =
      ZLayer.fromFunction((testRepo,sid) => TestRunnerImpl(testRepo,sid))
  }




