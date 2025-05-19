package app

import conf.{ConfigHelper, WebUiConfig}
import data.ImplTestsRepo
import web.WebUiApp
import zio.http.Server
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

/**
 * up_db\src\main\resources\control.conf
*/
object MainApp extends ZIOAppDefault{

  //https://zio.dev/zio-http/reference/server/
  //https://www.ziverge.com/post/how-to-implement-a-rest-api-in-scala-3-with-zio-http-magnum-and-iron
    private def startWebServer(): ZIO[WebUiConfig, Throwable, Unit] = for {
    config <- ZIO.service[WebUiConfig]
    server = Server.defaultWithPort(config.port)
    _ <-  (Server.install(WebUiApp.app).flatMap { port =>
      ZIO.logInfo(s"Started server on port: $port")
    } *> ZIO.never).provide(server, ImplTestsRepo.layer)
  } yield ()

  private val mainApp: ZIO[ZIOAppArgs, Throwable, Unit] = for {
    args <- ZIO.service[ZIOAppArgs]
    confLayer <- ConfigHelper.ConfigZLayer(args)
    _ <- startWebServer().provide(confLayer)
  } yield ()

    def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
      mainApp.foldZIO(
        err => ZIO.logError(s"Exception - ${err.getMessage}").as(0),
        suc => ZIO.succeed(suc)
      )

}
