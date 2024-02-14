lazy val commonSettings = Seq(
  scalaVersion := "2.12.18",
  version := "0.4-SNAPSHOT",
  organization := "de.b-studios",
  crossScalaVersions := Seq("2.12.18", "2.13.12", "3.3.2"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(
          "-Xfuture",
          "-Yno-adapted-args"
        )
      case _ =>
        Nil
    }
  },
  scalacOptions ++= {
    if (scalaBinaryVersion.value == "3") {
      Nil
    } else {
      Seq(
        "-Ywarn-dead-code",
        "-Ywarn-numeric-widen",
        "-Ywarn-value-discard"
      )
    }
  },
  run / fork := true,
  test / fork := true,
  Test / parallelExecution := false
)

lazy val micrositeSettings = Seq(
  micrositeName := "Scala Effekt",
  micrositeDescription := "Extensible algebraic effects with handlers",
  micrositeAuthor := "Jonathan Brachthäuser (@b-studios)",
  micrositeBaseUrl := "/scala-effekt",
  micrositeDocumentationUrl := "/scala-effekt/guides.html",
  micrositeGithubOwner := "b-studios",
  micrositeGithubRepo := "scala-effekt",
  micrositeHighlightTheme := "atom-one-light",
  micrositeOrganizationHomepage := "http://b-studios.de",
  // micrositePushSiteWith := GitHub4s,
  makeSite / includeFilter := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md",
  micrositePalette := greenTheme,
  micrositeCssDirectory := (Compile / resourceDirectory).value / "microsite" / "styles",
  micrositeJsDirectory := (Compile / resourceDirectory).value / "microsite" / "js",
  git.remoteRepo := "git@github.com:b-studios/scala-effekt.git"
)

lazy val docs = (project in file("docs"))
  .enablePlugins(MicrositesPlugin)
  .settings(moduleName := "docs")
  .settings(commonSettings)
  .settings(micrositeSettings)
  .settings(noPublishSettings)
  .dependsOn(effektJVM)

lazy val effektSettings = commonSettings ++ publishSettings

lazy val root = project
  .in(file("."))
  .settings(moduleName := "root")
  .settings(effektSettings)
  .settings(noPublishSettings)
  .aggregate(effektJVM, effektJS, effectsJVM, effectsJS)
  .dependsOn(effektJVM, effektJS, effectsJVM, effectsJS)

lazy val effekt = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(moduleName := "effekt", name := "effekt")
  .settings(effektSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings:_*)

lazy val effektJVM = effekt.jvm
lazy val effektJS = effekt.js

lazy val commonJvmSettings = Def.settings()

lazy val commonJsSettings = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  Global / scalaJSStage := FastOptStage
)

lazy val effects = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("effects"))
  .settings(moduleName := "effekt-effects", name := "effect instances using Effekt")
  .settings(effektSettings ++ effectsSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .dependsOn(effekt)

lazy val effectsJVM = effects.jvm
lazy val effectsJS = effects.js

lazy val effectsSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.10.0"
  )
)

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
  Test / publishArtifact := false,
  pomIncludeRepository := (_ => false),
  publishTo := {
      if (version.value.trim endsWith "SNAPSHOT")
        Some("Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")
      else
        Some("Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
  },
  homepage := Some(url("https://b-studios.de/scala-effekt")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/b-studios/scala-effekt"), "scm:git:git@github.com:b-studios/scala-effekt.git")),
  autoAPIMappings := true,
  apiURL := Some(url("https://b-studios.de/scala-effekt")),
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
  publish := {},
  publishLocal := {},
  publishArtifact := false
)
