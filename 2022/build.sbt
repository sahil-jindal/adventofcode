val scala3Version = "3.7.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2022",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    Global / semanticdbEnabled := true,
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.5",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
