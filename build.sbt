organization in ThisBuild := "de.envisia.erp.dsl"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.2"
)

lazy val `erp-dsl` = (project in file("."))
    .settings(commonSettings)
    .settings(libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4")
    .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % Test)




