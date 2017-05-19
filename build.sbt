lazy val commonSettings = Seq(
  scalaVersion := "2.12.2",
  crossScalaVersions := Seq("2.11.2", "2.11.8"),
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
  micrositeTwitterCreator := "@__protected",
  micrositeBaseUrl := "/effekt",
  // micrositeDocumentationUrl := "/effekt/docs/",
  micrositeGithubOwner := "b-studios",
  micrositeGithubRepo := "scala-effekt",
  micrositeGithubToken := getEnvVar("GITHUB_TOKEN")
  micrositeHighlightTheme := "atom-one-light",
  micrositeOrganizationHomepage := "http://b-studios.de",
  // micrositePushSiteWith := GitHub4s,
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md",
  micrositePalette := greenTheme,
  fork in tut := true
)

lazy val docs = (project in file("docs"))
  .enablePlugins(MicrositesPlugin)
  .settings(moduleName := "docs")
  .settings(commonSettings)
  .settings(micrositeSettings)
  .dependsOn(effekt)

lazy val effekt = project.in(file("."))
  .settings(moduleName := "root")
  .settings(commonSettings)

lazy val greenTheme = Map(
    "brand-primary" -> "#3C8875",
    "brand-secondary" -> "#17534A",
    "brand-tertiary" -> "#134946",
    "gray-dark" -> "#394A4B",
    "gray" -> "#4F5B5C",
    "gray-light" -> "#CFE4E4",
    "gray-lighter" -> "#F4F4F4",
    "white-color" -> "#FFFFFF")
