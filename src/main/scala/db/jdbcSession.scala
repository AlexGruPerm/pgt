package db

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import tmodel.TestsMeta
import zio._
import error.DatabaseError // Import DatabaseError

import java.sql.{Connection, ResultSet, Statement}
// Removed Properties import as it's no longer directly used for connection creation

case class pgSess(sess: Connection) {
  def getPid: ZIO[Any, DatabaseError, Int] = ZIO.attemptBlocking {
    val stmt: Statement = sess.createStatement
    val rs: ResultSet = stmt.executeQuery("SELECT pg_backend_pid() as pg_backend_pid")
    rs.next()
    val pg_backend_pid: Int = rs.getInt("pg_backend_pid")
    pg_backend_pid
  }.mapError(e => DatabaseError(s"Failed to get pg_backend_pid: ${e.getMessage}", Some(e)))
}

trait jdbcSession {
  def pgConnection(testId: Int): ZIO[Scope, DatabaseError, pgSess] // Changed return type to include Scope for proper resource handling
  def getMaxConnections(connection: pgSess): ZIO[Any, DatabaseError, Int]
}

case class jdbcSessionImpl(dataSource: HikariDataSource) extends jdbcSession {

  override def pgConnection(testId: Int): ZIO[Scope, DatabaseError, pgSess] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking {
        val conn = dataSource.getConnection()
        conn.setClientInfo("ApplicationName", s"up_test $testId")
        conn.setAutoCommit(false) // It's common to set this, but ensure it's what you want for all pooled connections
        pgSess(conn)
      }.mapError(e => DatabaseError(s"Failed to acquire PG connection: ${e.getMessage}", Some(e)))
    )(sess => ZIO.succeedBlocking(sess.sess.close())) // Release: close connection to return to pool
      .tap(sess => sess.getPid.flatMap(pid => ZIO.logInfo(s"Acquired connection for testId $testId. pg_backend_pid = $pid")).unlessZIO(ZIO.fiberId.map(_.isNone))) // Log only if not interrupted

  def getMaxConnections(connection: pgSess): ZIO[Any, DatabaseError, Int] =
    ZIO.attemptBlocking {
      // connection.sess.setAutoCommit(false) // Usually not needed here if primarily reading settings
      val rs: ResultSet = connection.sess.createStatement.executeQuery(
        """ SELECT setting
          | FROM   pg_settings
          | WHERE  name = 'max_connections' """.stripMargin)
      rs.next()
      rs.getInt("setting")
    }.mapError(e => DatabaseError(s"Exception in getMaxConnections: ${e.getMessage}", Some(e)))
}

object jdbcSessionImpl {
  private def createDataSource(testMeta: TestsMeta): ZIO[Scope, DatabaseError, HikariDataSource] =
    ZIO.acquireRelease(
      ZIO.attempt {
        val config = new HikariConfig()
        config.setJdbcUrl(testMeta.url)
        config.setUsername(testMeta.db_user)
        config.setPassword(testMeta.db_password)
        // config.setDriverClassName("org.postgresql.Driver") // HikariCP usually infers this from jdbcUrl
        // Add other HikariCP specific configurations here if needed, e.g., pool size
        // config.setMaximumPoolSize(10)
        new HikariDataSource(config)
      }.mapError(e => DatabaseError(s"Failed to create HikariDataSource: ${e.getMessage}${testMeta.urlMsg}", Some(e)))
    )(ds => ZIO.succeedBlocking(ds.close()))

  val layer: ZLayer[TestsMeta, DatabaseError, jdbcSession] = // Changed Exception to DatabaseError
    ZLayer {
      for {
        testMeta <- ZIO.service[TestsMeta]
        dataSource <- createDataSource(testMeta) // Use the scoped resource for DataSource
      } yield jdbcSessionImpl(dataSource)
    }
}