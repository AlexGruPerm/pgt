name := "pgt"

ThisBuild / organization := "yakushev"
ThisBuild / version      := "0.0.1"
ThisBuild / scalaVersion := "2.13.16"

  val Versions = new {
    val zio         = "2.1.21"
    val zio_config  = "4.0.5"
    val zio_http    = "3.5.1"
    val zio_json    = "0.7.44"
    val pgVers      = "42.7.8"
    val hikari      = "7.0.2"
    val poi         = "5.4.1"
  }

  lazy val global = project
  .in(file("."))
  .settings(
    name := "pgt",
    Compile / mainClass := Some("app.MainApp"),
    assembly / assemblyJarName := "pg_test.jar",
    commonSettings,
    libraryDependencies ++= commonDependencies,
    assembly / assemblyMergeStrategy := {
      //case PathList("module-info.class") => MergeStrategy.discard
      //case x if x.endsWith("/module-info.class") => MergeStrategy.discard
      //case PathList("META-INF", xs @ _*)         => MergeStrategy.discard
      //case "reference.conf" => MergeStrategy.concat
      //case _ => MergeStrategy.first
      // Netty versions.properties — берём первый
      case PathList("META-INF", "io.netty.versions.properties")     => MergeStrategy.first
      case PathList("META-INF/versions/11/", "module-info.class")   => MergeStrategy.first
      // Конфиги — склеиваем
      case PathList("reference.conf")                  => MergeStrategy.concat
      case PathList("application.conf")                => MergeStrategy.concat
      case PathList("META-INF", "services", _ @ _*)    => MergeStrategy.filterDistinctLines
      // Модульные дескрипторы Jigsaw — отбрасываем
      case PathList("module-info.class")               => MergeStrategy.discard
      case PathList("META-INF", "versions", _ @ _*)    => MergeStrategy.discard
      // Стандартные META-INF мусор/подписи — отбрасываем
      case PathList("META-INF", "MANIFEST.MF")         => MergeStrategy.discard
      case PathList("META-INF", "INDEX.LIST")          => MergeStrategy.discard
      case PathList("META-INF", "DEPENDENCIES")        => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.nonEmpty && xs.last.toLowerCase.endsWith(".sf")  => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.nonEmpty && xs.last.toLowerCase.endsWith(".dsa") => MergeStrategy.discard

      // Остальное по умолчанию
      case x =>
        val old = (assembly / assemblyMergeStrategy).value
        old(x)
    }
  )

  lazy val dependencies =
    new {
      val zio = "dev.zio" %% "zio" % Versions.zio

      val zio_conf          = "dev.zio" %% "zio-config"          % Versions.zio_config
      val zio_conf_typesafe = "dev.zio" %% "zio-config-typesafe" % Versions.zio_config
      val zio_conf_magnolia = "dev.zio" %% "zio-config-magnolia" % Versions.zio_config

      val zio_http = "dev.zio" %% "zio-http" % Versions.zio_http
      val zio_json = "dev.zio" %% "zio-json" % Versions.zio_json

      val zioDep = List(zio, zio_conf, zio_conf_typesafe, zio_conf_magnolia, zio_http, zio_json)

      val poi = "org.apache.poi" % "poi" % Versions.poi
      val poi_ooxml = "org.apache.poi" % "poi-ooxml" % Versions.poi

      val pg = "org.postgresql" % "postgresql" % Versions.pgVers
      val hikaricp = "com.zaxxer" % "HikariCP" % Versions.hikari
      val dbDep = List(pg,hikaricp, poi,poi_ooxml)
    }

  val commonDependencies = {
    dependencies.zioDep ++ dependencies.dbDep
  }

  lazy val compilerOptions = Seq(
          "-deprecation",
          "-encoding", "utf-8",
          "-explaintypes",
          "-feature",
          "-unchecked",
          "-language:postfixOps",
          "-language:higherKinds",
          "-language:implicitConversions",
          "-Xcheckinit",
          "-Xfatal-warnings",
          "-Ywarn-unused:params,-implicits"
  )

  lazy val commonSettings = Seq(
    scalacOptions ++= compilerOptions,
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Snapshots s01" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
      Resolver.DefaultMavenRepository,
      Resolver.mavenLocal,
      Resolver.bintrayRepo("websudos", "oss-releases")
    )++
      Resolver.sonatypeOssRepos("snapshots")
     ++ Resolver.sonatypeOssRepos("public")
     ++ Resolver.sonatypeOssRepos("releases")
  )
