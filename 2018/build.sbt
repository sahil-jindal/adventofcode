val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2018",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    Global / semanticdbEnabled := true,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "3.0.0"
  )
