import ReleaseTransformations.*

ThisBuild / organization := "de.envisia.erp"

lazy val commonSettings = Seq(
  scalaVersion := "3.3.1",
  Compile /  doc / scalacOptions ++= Seq(
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
        "org.parboiled" %% "parboiled" % "2.5.1",
        "org.scalatest" %% "scalatest" % "3.2.17" % Test
      )
    )


releasePublishArtifactsAction := PgpKeys.publishSigned.value
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts,
  setNextVersion,
  commitNextVersion,
  pushChanges
)