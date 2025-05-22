package db

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import tmodel.TestsMeta
import zio._

import java.sql.{Connection, DriverManager, ResultSet, SQLException, Statement}
import java.util.Properties


case class pgSess(sess : Connection){

/*  def getPid: Int = {
    val stmt: Statement = sess.createStatement
    val rs: ResultSet = stmt.executeQuery("SELECT pg_backend_pid() as pg_backend_pid")
    rs.next()
    val pg_backend_pid: Int = rs.getInt("pg_backend_pid")
    pg_backend_pid
  }*/

  def getPid: ZIO[Any, SQLException, Int] = ZIO.attemptBlocking {
    val stmt: Statement = sess.createStatement
    val rs: ResultSet = stmt.executeQuery("SELECT pg_backend_pid() as pg_backend_pid")
    rs.next()
    val pg_backend_pid: Int = rs.getInt("pg_backend_pid")
    pg_backend_pid
  }.refineToOrDie[SQLException]//.mapError(e => SQLException(s"Failed to get pg_backend_pid: ${e.getMessage}", Some(e)))

}

trait jdbcSession {
/*  val props = new Properties()
  def pgConnection(testId: Int): ZIO[Any,Exception,pgSess]*/
  def pgConnection(testId: Int): ZIO[Scope, SQLException, pgSess] // Changed return type to include Scope for proper resource handling
  def getMaxConnections(connection: pgSess): ZIO[Any, SQLException, Int]
}

case class jdbcSessionImpl(dataSource: HikariDataSource/*cp: TestsMeta*/) extends jdbcSession {

  override def pgConnection(testId: Int): ZIO[Scope, SQLException, pgSess] =
    ZIO.acquireRelease(
        ZIO.attemptBlocking {
          println("Begin create connection inside pgConnection")
          val conn = dataSource.getConnection()
          println("-- after dataSource.getConnection() --")
          println(s"conn.isClosed = ${conn.isClosed}")
          conn.setClientInfo("ApplicationName", s"up_test $testId")
          conn.setAutoCommit(false) // It's common to set this, but ensure it's what you want for all pooled connections
          val s = pgSess(conn)
          println(s"sess created inside pgConnection pid = ${s.getPid}")
          s
        }.refineToOrDie[SQLException]
      )(sess => ZIO.succeedBlocking(sess.sess.close())) // Release: close connection to return to pool
      .tap(sess => sess.getPid.flatMap(pid => ZIO.logInfo(s"Acquired connection for testId $testId. pg_backend_pid = $pid"))
        //.unlessZIO(ZIO.fiberId.map(_.isNone))
      ) // Log only if not interrupted

  def getMaxConnections(connection: pgSess): ZIO[Any, SQLException, Int] =
    ZIO.attemptBlocking {
      // connection.sess.setAutoCommit(false) // Usually not needed here if primarily reading settings
      val rs: ResultSet = connection.sess.createStatement.executeQuery(
        """ SELECT setting
          | FROM   pg_settings
          | WHERE  name = 'max_connections' """.stripMargin)
      rs.next()
      rs.getInt("setting")
    }.refineToOrDie[SQLException]

/*  override def pgConnection(testId: Int):  ZIO[Any,Exception,pgSess] = for {
    _ <- ZIO.unit
    sessEffect = ZIO.attemptBlocking{
        props.setProperty("user", cp.db_user)
        props.setProperty("password", cp.db_password)
        val conn = DriverManager.getConnection(cp.url, props)
        conn.setClientInfo("ApplicationName",s"up_test $testId")
        conn.setAutoCommit(false)
        pgSess(conn)
      }.catchAll {
      case e: Exception => ZIO.logError(e.getMessage) *>
        ZIO.fail(new Exception(e.getMessage +cp.urlMsg))
    }
    _ <- ZIO.logInfo(s"  ") *>
      ZIO.logInfo(s"New connection =============== >>>>>>>>>>>>> ")
    sess <- sessEffect
    _ <- ZIO.logInfo(s"pg_backend_pid = ${sess.getPid}")
  } yield sess

  def getMaxConnections(connection: pgSess): ZIO[Any,Exception,Int] =
    for {
      maxConn <- ZIO.attemptBlocking{
        connection.sess.setAutoCommit(false)
        //setting as MAXCONN
        val rs: ResultSet = connection.sess.createStatement.executeQuery(
          """ SELECT setting
            | FROM   pg_settings
            | WHERE  name = 'max_connections' """.stripMargin)
        rs.next()
        rs.getInt("setting")
      }.catchAll {
        case e: Exception => ZIO.logError(s" Exception getMaxConnections msg=${e.getMessage}") *>
          ZIO.fail(throw new Exception(e.getMessage +cp.urlMsg))
      }
    } yield maxConn*/

}

object jdbcSessionImpl {

  private def createDataSource(testMeta: TestsMeta): ZIO[Scope, SQLException, HikariDataSource] =
    ZIO.acquireRelease(
      ZIO.attempt {
        val config = new HikariConfig()
        config.setJdbcUrl(testMeta.url)
        config.setUsername(testMeta.db_user)
        config.setPassword(testMeta.db_password)
        // config.setDriverClassName("org.postgresql.Driver") // HikariCP usually infers this from jdbcUrl
        // Add other HikariCP specific configurations here if needed, e.g., pool size
        config.setMaximumPoolSize(1)
        val hds = new HikariDataSource(config)
        println(s"HikariCP created, Pool is opened = ${!hds.isClosed} MaximumPoolSize = ${hds.getMaximumPoolSize}")
        hds
      }.refineToOrDie[SQLException]//.mapError(e => DatabaseError(s"Failed to create HikariDataSource: ${e.getMessage}${testMeta.urlMsg}", Some(e)))
    )(ds => ZIO.succeedBlocking(ds.close()))

  val layer: ZLayer[TestsMeta, SQLException, jdbcSession] =
    ZLayer.scoped {
      for {
        testMeta <- ZIO.service[TestsMeta]
        session <- ZIO.acquireRelease(createDataSource(testMeta).map(ds => jdbcSessionImpl(ds))) {
          case jdbcSessionImpl(ds) => ZIO.attempt(ds.close()).orDie
        }
      } yield session
    }

/*  val layer: ZLayer[TestsMeta, SQLException, jdbcSession] =
    ZLayer {
      for {
        testMeta <- ZIO.service[TestsMeta]
        session <- ZIO.scoped {  // Локально используем Scope, но не пробрасываем его наружу
          ZIO.logInfo("jdbcSession.layer -> creating createDataSource") *>
          createDataSource(testMeta).map(ds => jdbcSessionImpl(ds))
        }
      } yield session
    }*/

/*  val layer: ZLayer[TestsMeta, SQLException, jdbcSession] =
    ZLayer.scoped {  // Используем scoped для работы с ресурсами
      for {
        testMeta <- ZIO.service[TestsMeta]
        dataSource <- createDataSource(testMeta)  // Пробрасываем контекст
      } yield jdbcSessionImpl(dataSource)  // Предполагается, что jdbcSessionImpl принимает HikariDataSource
    }*/

/*  val layer: ZLayer[TestsMeta, SQLException, jdbcSession] = // Changed Exception to DatabaseError
    ZLayer {
      for {
        testMeta <- ZIO.service[TestsMeta]
        dataSource <- createDataSource(testMeta) // Use the scoped resource for DataSource
      } yield jdbcSessionImpl(dataSource)
    }*/

/*  val layer: ZLayer[TestsMeta, Exception, jdbcSession] =
    ZLayer{
      for {
        testMeta <- ZIO.service[TestsMeta]
      } yield jdbcSessionImpl(testMeta)
    }*/

}