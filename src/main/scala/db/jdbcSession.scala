package db

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import tmodel.TestsMeta
import zio._
import java.sql.{Connection, ResultSet, SQLException, Statement}

case class pgSess(sess : Connection){

  def getPid: ZIO[Any, SQLException, Int] = ZIO.attemptBlocking {
    val stmt: Statement = sess.createStatement
    val rs: ResultSet = stmt.executeQuery("SELECT pg_backend_pid() as pg_backend_pid")
    rs.next()
    val pg_backend_pid: Int = rs.getInt("pg_backend_pid")
    println(s"PID = $pg_backend_pid")
    pg_backend_pid
  }.refineToOrDie[SQLException]

}

trait jdbcSession {
  def pgConnection(testId: Int): ZIO[Scope, SQLException, pgSess] // Changed return type to include Scope for proper resource handling
  def getMaxConnections(connection: pgSess): ZIO[Any, SQLException, Int]
}

case class jdbcSessionImpl(dataSource: HikariDataSource) extends jdbcSession {

  override def pgConnection(testId: Int): ZIO[Scope, SQLException, pgSess] =
    ZIO.acquireRelease(
        ZIO.attemptBlocking {
          val conn = dataSource.getConnection()
          conn.setClientInfo("ApplicationName", s"up_test $testId")
          conn.setAutoCommit(false)
          val stmt = conn.createStatement()
          stmt.execute("RESET ALL;")
          val s = pgSess(conn)
          s
        }.refineToOrDie[SQLException]
      )(sess => ZIO.succeedBlocking(sess.sess.close())) // Release: close connection to return to pool
      .tap(sess => sess.getPid.flatMap(pid => ZIO.logInfo(s"Acquired conn. for testId $testId. pid = $pid"))
        .unlessZIO(ZIO.fiberId.map(_.isNone))
      )

  def getMaxConnections(connection: pgSess): ZIO[Any, SQLException, Int] =
    ZIO.attemptBlocking {
      val rs: ResultSet = connection.sess.createStatement.executeQuery(
        """ SELECT setting
          | FROM   pg_settings
          | WHERE  name = 'max_connections' """.stripMargin)
      rs.next()
      rs.getInt("setting")
    }.refineToOrDie[SQLException]
}

object jdbcSessionImpl {

  private def createDataSource(testMeta: TestsMeta): ZIO[Any, SQLException, HikariDataSource] =
      ZIO.attempt {
        val config = new HikariConfig()
        config.setJdbcUrl(testMeta.url)
        config.setUsername(testMeta.db_user)
        config.setPassword(testMeta.db_password)
        config.setMaximumPoolSize(1)
        println(s"createDataSource url = ${testMeta.urlMsg}")
        val hds = new HikariDataSource(config)
        println(" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ")
        println(" ")
        println(s"     HikariCP created, pool is opened = ${!hds.isClosed} MaximumPoolSize = ${hds.getMaximumPoolSize}")
        println(" ")
        println(" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ")
        hds
      }.tapError {
          case e: Exception => ZIO.logError(s"createDataSource err = ${e.getMessage}")
        }
        .refineToOrDie[SQLException]

  val layer: ZLayer[TestsMeta, SQLException, jdbcSession] =
    ZLayer.scoped {
      for {
        testMeta <- ZIO.service[TestsMeta]
        session <- ZIO.acquireRelease(createDataSource(testMeta).map(ds => jdbcSessionImpl(ds))) {
          case jdbcSessionImpl(ds) => ZIO.attempt{
            ds.close()
            println(" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ")
            println(" ")
            println(s"     Closing HikariCP, pool is closed = ${ds.isClosed}")
            println(" ")
            println(" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ")
            }.orDie
        }
      } yield session
    }
}