package conf

import com.typesafe.config.{Config, ConfigFactory}
import error.ConfigError
import zio.{ZIO, ZIOAppArgs, ZLayer}
import zio.test._
import zio.test.Assertion._

object ConfigHelperSpec extends ZIOSpecDefault {

  def spec = suite("ConfigHelperSpec")(
    suite("getConfig method")(
      test("should load WebUiConfig successfully when port is present") {
        val configString = """webui.port = 8080"""
        val typesafeConfig = ConfigFactory.parseString(configString)
        val result = ConfigHelper.getConfig(typesafeConfig)
        assert(result)(isRight(equalTo(WebUiConfig(8080))))
      },
      test("should return ConfigError when port is missing") {
        val configString = """webui.another_key = "value" """
        val typesafeConfig = ConfigFactory.parseString(configString)
        val result = ConfigHelper.getConfig(typesafeConfig)
        assert(result)(isLeft(isSubtype[ConfigError](anything)))
      },
      test("should return ConfigError when port is of wrong type") {
        val configString = """webui.port = "not-an-int" """
        val typesafeConfig = ConfigFactory.parseString(configString)
        val result = ConfigHelper.getConfig(typesafeConfig)
        assert(result)(isLeft(isSubtype[ConfigError](anything)))
      }
    ),
    suite("ConfigZLayer method")(
      test("should fail with ConfigError if no command line arguments are provided") {
        val args = ZIOAppArgs.empty
        val result = ConfigHelper.ConfigZLayer(args).exit
        assertZIO(result)(fails(isSubtype[ConfigError](hasField("message", _.message, equalTo("Empty parameters. Please provide input config file.")))))
      },
      test("should load config successfully from a valid file path") {
        // Create a temporary config file
        val tempFile = java.io.File.createTempFile("test-config", ".conf")
        val writer = new java.io.PrintWriter(tempFile)
        try {
          writer.println("webui.port = 9999")
        } finally {
          writer.close()
        }

        val args = ZIOAppArgs.make(Chunk(tempFile.getAbsolutePath))
        val layer = ConfigHelper.ConfigZLayer(args).flatten
        val result = ZIO.service[WebUiConfig].provideLayer(layer)

        assertZIO(result.map(_.port))(equalTo(9999))
          .ensuring(ZIO.succeed(tempFile.delete())) // Clean up
      },
      test("should fail with an error if config file does not exist (via config method)") {
        val nonExistentFilePath = "non-existent-test-config.conf"
        val args = ZIOAppArgs.make(Chunk(nonExistentFilePath))
        
        // ConfigZLayer wraps the config loading. The actual file parsing happens in the `config` val.
        // We expect a failure during the resolution of the layer.
        val layerEffect = ConfigHelper.ConfigZLayer(args).flatten
        val result = ZIO.service[WebUiConfig].provideLayer(layerEffect).exit

        // The ConfigFactory.parseFile itself will throw a ConfigException if file not found.
        // This is now mapped to ConfigError in `ConfigHelper.config`.
        assertZIO(result)(fails(isSubtype[ConfigError](
          hasField("message", (e: ConfigError) => e.message, startsWithString(s"Failed to parse config file $nonExistentFilePath"))
        )))
      }
    )
  )
}
