lazy val commonSettings = Seq(
  scalaVersion := "2.12.2",
  version := "0.1-SNAPSHOT",
  organization := "de.b-studios",
  crossScalaVersions := Seq("2.11.8", "2.12.1"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  fork in test := true,
  parallelExecution in Test := false
)


lazy val micrositeSettings = Seq(
  micrositeName := "Scala Effekt",
  micrositeDescription := "Extensible algebraic effects with handlers",
  micrositeAuthor := "Jonathan Brachthäuser (@b-studios)",
  micrositeBaseUrl := "/scala-effekt",
  // micrositeDocumentationUrl := "/effekt/docs/",
  micrositeGithubOwner := "b-studios",
  micrositeGithubRepo := "scala-effekt",
  micrositeHighlightTheme := "atom-one-light",
  micrositeOrganizationHomepage := "http://b-studios.de",
  // micrositePushSiteWith := GitHub4s,
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md",
  micrositePalette := greenTheme,
  fork in tut := true,
  git.remoteRepo := "git@github.com:b-studios/scala-effekt.git"
)

lazy val docs = (project in file("docs"))
  .enablePlugins(MicrositesPlugin)
  .settings(moduleName := "docs")
  .settings(commonSettings)
  .settings(micrositeSettings)
  .dependsOn(effekt)

lazy val effekt = project.in(file("."))
  .settings(moduleName := "effekt")
  .settings(commonSettings)
  .settings(publishSettings)

lazy val greenTheme = Map(
    "brand-primary" -> "#469E6B",
    "brand-secondary" -> "#175341",
    "brand-tertiary" -> "#103D33",
    "gray-dark" -> "#394A4B",
    "gray" -> "#4F5B5C",
    "gray-light" -> "#CFE4E4",
    "gray-lighter" -> "#F4F4F4",
    "white-color" -> "#FFFFFF")

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
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
