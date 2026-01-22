val scala3Version = "3.8.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2024",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    Global / semanticdbEnabled := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
