val scala3Version = "3.8.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2016",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    Global / semanticdbEnabled := true,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
