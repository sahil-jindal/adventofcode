val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2015",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    Global / semanticdbEnabled := true,
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.4",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
