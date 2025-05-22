package conf

import zio.{ZIO, ZIOAppArgs, ZLayer}
import com.typesafe.config.{Config, ConfigFactory}
import java.io
import java.io.File
import error.AppError // Import AppError
import error.ConfigError // Import ConfigError

final case class WebUiConfig(port: Int)

case object ConfigHelper {

  def getConfig(fileConfig: Config): Either[ConfigError, WebUiConfig] = {
    val webui = "webui."
    try {
      Right(WebUiConfig(
        port = fileConfig.getInt(webui + "port")
      ))
    } catch {
      case e: com.typesafe.config.ConfigException => Left(ConfigError(s"Configuration error: ${e.getMessage}"))
    }
  }

  val config: ZIO[String, AppError, WebUiConfig] =
    for {
      configParam <- ZIO.service[String]
      configFilename: String = System.getProperty("user.dir") + File.separator + configParam
      fileConfig <- ZIO.attempt(ConfigFactory.parseFile(new io.File(configFilename)))
        .mapError(e => ConfigError(s"Failed to parse config file $configFilename: ${e.getMessage}"))
      appConfig <- ZIO.fromEither(ConfigHelper.getConfig(fileConfig))
    } yield appConfig

  def ConfigZLayer(confParam: ZIOAppArgs): ZIO[Any, AppError, ZLayer[Any, AppError, WebUiConfig]] = for {
    _ <- ZIO.fail(ConfigError("Empty parameters. Please provide input config file."))
      .when(confParam.getArgs.isEmpty)
    appCfg = ZLayer {
      for {
        cfg <- confParam.getArgs.toList match {
          case List(configFile) => config.provide(ZLayer.succeed(configFile))
          case _ => ZIO.fail(ConfigError("Empty parameters. Please provide input config file.")) // Changed Exception to ConfigError
        }
      } yield cfg
    }
  } yield appCfg

}
