lazy val root = project
  .in(file("."))
  .settings(moduleName := "effekt", name := "effekt")
  .settings(Seq(
    scalaVersion := dottyLatestNightlyBuild.get,
    version := "0.2-SNAPSHOT",
    organization := "de.b-studios",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-language:implicitConversions"
    ),
    fork in test := true,
    parallelExecution in Test := false
  ))
