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
    pg_backend_pid
  }.refineToOrDie[SQLException]

}

trait jdbcSession {
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
          conn.setAutoCommit(false)
          val s = pgSess(conn)
          println(s"sess created inside pgConnection pid = ${s.getPid}")
          s
        }.refineToOrDie[SQLException]
      )(sess => ZIO.succeedBlocking(sess.sess.close())) // Release: close connection to return to pool
      .tap(sess => sess.getPid.flatMap(pid => ZIO.logInfo(s"Acquired connection for testId $testId. pg_backend_pid = $pid"))
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
        val hds = new HikariDataSource(config)
        println(s"HikariCP created, Pool is opened = ${!hds.isClosed} MaximumPoolSize = ${hds.getMaximumPoolSize}")
        hds
      }.refineToOrDie[SQLException]

  val layer: ZLayer[TestsMeta, SQLException, jdbcSession] =
    ZLayer.scoped {
      for {
        testMeta <- ZIO.service[TestsMeta]
        session <- ZIO.acquireRelease(createDataSource(testMeta).map(ds => jdbcSessionImpl(ds))) {
          case jdbcSessionImpl(ds) => ZIO.attempt(ds.close()).orDie
        }
      } yield session
    }
}