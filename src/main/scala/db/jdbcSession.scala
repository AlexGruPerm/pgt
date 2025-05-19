package db

import tmodel.TestsMeta
import zio.{Task, _}

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import java.util.Properties


case class pgSess(sess : Connection){

  def getPid: Int = {
    val stmt: Statement = sess.createStatement
    val rs: ResultSet = stmt.executeQuery("SELECT pg_backend_pid() as pg_backend_pid")
    rs.next()
    val pg_backend_pid: Int = rs.getInt("pg_backend_pid")
    pg_backend_pid
  }

}

trait jdbcSession {
  val props = new Properties()
  def pgConnection(testId: Int): ZIO[Any,Exception,pgSess]
}

case class jdbcSessionImpl(cp: TestsMeta) extends jdbcSession {
  override def pgConnection(testId: Int):  ZIO[Any,Exception,pgSess] = for {
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
    } yield maxConn

}

object jdbcSessionImpl {

  val layer: ZLayer[TestsMeta, Exception, jdbcSession] =
    ZLayer{
      for {
        testMeta <- ZIO.service[TestsMeta]
      } yield jdbcSessionImpl(testMeta)
    }

}