import Dependencies._

lazy val root = (project in file(".")).enablePlugins(JmhPlugin).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello"

  )

//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.3"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"
