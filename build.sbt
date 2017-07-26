import sbtrelease.ReleaseStateTransformations._

organization in ThisBuild := "de.envisia.erp"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.2",
  scalacOptions in(Compile, doc) ++= Seq(
    "-target:jvm-1.8",
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation"
  ),
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  publishTo := Some("envisia-internal" at "https://nexus.envisia.de/repository/internal/")
)

lazy val `expression-dsl` = (project in file("."))
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.parboiled" %% "parboiled" % "2.1.4",
        "org.scalatest" %% "scalatest" % "3.0.3" % Test
      )
    )


releaseCrossBuild := true
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _), enableCrossBuild = true),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

