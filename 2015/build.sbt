val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2015",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    Global / semanticdbEnabled := true,
    libraryDependencies ++= Seq(
      "org.playframework" %% "play-json" % "3.0.6",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
