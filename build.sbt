lazy val commonSettings = Seq(
  scalaVersion := "0.8.0-RC1",
  version := "0.1.1-SNAPSHOT",
  organization := "de.b-studios",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked"
  ),
  fork in test := true,
  parallelExecution in Test := false,
  mainClass in (Compile, run) := Some("effekt.examples.shallow.fluent")
)


lazy val effektSettings = commonSettings ++ publishSettings

lazy val root = project
  .in(file("."))
  .settings(moduleName := "effekt", name := "effekt")
  .settings(effektSettings)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishArtifact in (Compile, packageDoc) := false,
  pomIncludeRepository := (_ => false),
  publishTo := {
      if (version.value.trim endsWith "SNAPSHOT")
        Some("Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")
      else
        Some("Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
  },
  homepage := Some(url("http://b-studios.de/scala-effekt")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/b-studios/scala-effekt"), "scm:git:git@github.com:b-studios/scala-effekt.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://b-studios.de/scala-effekt")),
  pomExtra := (
    <developers>
      <developer>
        <id>b-studios</id>
        <name>Jonathan Brachthäuser</name>
        <url>https://github.com/b-studios/</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)
