ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

addCompilerPlugin(
  "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full
)

lazy val root = (project in file("."))
  .settings(
    name := "npm-resolver",
    libraryDependencies ++= Seq(
      "io.7mind.izumi" %% "distage-core" % "1.1.0-M11",
      "io.7mind.izumi" %% "fundamentals-platform" % "1.1.0-M11",
      "dev.zio" %% "zio" % "1.0.17",
      "dev.zio" %% "zio-interop-cats" % "13.0.0.1",
      "org.http4s" %% "http4s-ember-server" % "0.23.16",
      "org.http4s" %% "http4s-ember-client" % "0.23.16",
      "org.http4s" %% "http4s-circe" % "0.23.16",
      "org.http4s" %% "http4s-dsl" % "0.23.16",
      "io.circe" %% "circe-parser" % "0.14.3",
      "io.circe" %% "circe-generic" % "0.14.3",
      "org.apache.commons" % "commons-compress" % "1.22"
    )
  )
