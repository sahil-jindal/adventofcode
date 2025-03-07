val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2015",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    Global / semanticdbEnabled := true,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test,
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.4"
  )
