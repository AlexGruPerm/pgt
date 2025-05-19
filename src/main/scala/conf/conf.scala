package conf

import zio.{ZIO, ZIOAppArgs, ZLayer}
import com.typesafe.config.{Config, ConfigFactory}
import java.io
import java.io.File

final case class WebUiConfig(port: Int)

case object ConfigHelper {

  def getConfig(fileConfig: Config): WebUiConfig = {
    val webui = "webui."
    WebUiConfig(
      port = fileConfig.getInt(webui + "port")
    )
  }

  val config: ZIO[String, Exception, WebUiConfig] =
    for {
      configParam <- ZIO.service[String]
      configFilename: String = System.getProperty("user.dir") + File.separator + configParam
      fileConfig = ConfigFactory.parseFile(new io.File(configFilename))
      appConfig = ConfigHelper.getConfig(fileConfig)
    } yield appConfig

  def ConfigZLayer(confParam: ZIOAppArgs): ZIO[Any, Exception, ZLayer[Any, Exception, WebUiConfig]] = for {
    _ <- ZIO.fail(new Exception("Empty parameters. Please provide input config file."))
      .when(confParam.getArgs.isEmpty)
    appCfg = ZLayer {
      for {
        cfg <- confParam.getArgs.toList match {
          case List(configFile) => config.provide(ZLayer.succeed(configFile))
          case _ => ZIO.fail(new Exception("Empty parameters. Please provide input config file."))
        }
      } yield cfg
    }
  } yield appCfg

}



