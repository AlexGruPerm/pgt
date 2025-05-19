name := "pgt"

ThisBuild / organization := "yakushev"
ThisBuild / version      := "0.0.1"
ThisBuild / scalaVersion := "2.13.16"

  val Versions = new {
    val zio         = "2.1.18"
    val zio_config  = "4.0.4"
    val zio_http    = "3.2.0"
    val zio_json    = "0.7.43"
    val pgVers      = "42.7.5"
  }

  lazy val global = project
  .in(file("."))
  .settings(
    name := "pgt",
    Compile / mainClass := Some("app.MainApp"),
    assembly / assemblyJarName := "gpt.jar",
    commonSettings,
    libraryDependencies ++= commonDependencies,
    assembly / assemblyMergeStrategy := {
      case PathList("module-info.class") => MergeStrategy.discard
      case x if x.endsWith("/module-info.class") => MergeStrategy.discard
      case PathList("META-INF", xs @ _*)         => MergeStrategy.discard
      case "reference.conf" => MergeStrategy.concat
      case _ => MergeStrategy.first
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

      val pg = "org.postgresql" % "postgresql" % Versions.pgVers

      val dbDep = List(pg)
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