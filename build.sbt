import xerial.sbt.Sonatype._

import com.scalapenos.sbt.prompt.SbtPrompt.autoImport._

promptTheme := com.scalapenos.sbt.prompt.PromptThemes.ScalapenosTheme

val commonSettings = Seq(
  // We are overriding the default behavior of sbt-git which, by default,
  // only appends the `-SNAPSHOT` suffix if there are uncommitted
  // changes in the workspace.
  version := {
    // Avoid Cyclic reference involving error
    if (git.gitCurrentTags.value.isEmpty || git.gitUncommittedChanges.value)
      git.gitDescribedVersion.value.get + "-SNAPSHOT"
    else
      git.gitDescribedVersion.value.get
  },
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12", "2.12.8"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    "locationtech-releases" at "https://repo.locationtech.org/content/groups/releases",
    "locationtech-snapshots" at "https://repo.locationtech.org/content/groups/snapshots"
  ),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  scalacOptions := Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:implicitConversions",
    "-language:reflectiveCalls",
    "-language:higherKinds",
    "-language:postfixOps",
    "-language:existentials",
    "-language:experimental.macros",
    "-feature",
    "-Ypatmat-exhaust-depth", "100"
  )
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

lazy val publishSettings = Seq(
  organization := "com.azavea.geotrellis",
  organizationName := "GeoTrellis",
  organizationHomepage := Some(new URL("https://geotrellis.io/")),
  description := "MAML is used to create a declarative structure that describes a combination of map algebra operations.",
  publishArtifact in Test := false
) ++ sonatypeSettings ++ credentialSettings

lazy val sonatypeSettings = Seq(
  publishMavenStyle := true,

  sonatypeProfileName := "com.azavea",
  sonatypeProjectHosting := Some(GitHubHosting(user="geotrellis", repository="maml", email="systems@azavea.com")),
  developers := List(
    Developer(id = "moradology", name = "Nathan Zimmerman", email = "nzimmerman@azavea.com", url = url("https://github.com/moradology")),
    Developer(id = "echeipesh", name = "Eugene Cheipesh", email = "echeipesh@azavea.com", url = url("https://github.com/echeipesh")),
    Developer(id = "lossyrob", name = "Rob Emanuele", email = "remanuele@azavea.com", url = url("https://github.com/lossyrob"))
  ),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),

  publishTo := sonatypePublishTo.value
)

lazy val credentialSettings = Seq(
  credentials += Credentials(
    "GnuPG Key ID",
    "gpg",
    System.getenv().get("GPG_KEY_ID"),
    "ignored"
  ),

  credentials += Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    System.getenv().get("SONATYPE_USERNAME"),
    System.getenv().get("SONATYPE_PASSWORD")
  )
)

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(publishSettings) // these settings are needed to release all aggregated modules under this root module
  .settings(noPublishSettings) // this is to exclue the root module itself from being published
  .aggregate(mamlJs, mamlJvm, mamlSpark)
  .enablePlugins(ScalaJSPlugin)

val circeVer       = "0.11.1"
val circeOpticsVer = "0.11.0"
val gtVer          = "3.0.0-M3"

lazy val maml = crossProject.in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck"             %% "scalacheck"           % "1.13.4" % "test",
      "org.scalatest"              %% "scalatest"            % "3.0.1"  % "test",
      "com.typesafe.scala-logging" %% "scala-logging"        % "3.9.0",
      "io.circe"                   %% "circe-core"           % circeVer,
      "io.circe"                   %% "circe-generic"        % circeVer,
      "io.circe"                   %% "circe-generic-extras" % circeVer,
      "io.circe"                   %% "circe-parser"         % circeVer,
      "io.circe"                   %% "circe-optics"         % circeOpticsVer
    )
  ).jvmSettings(
    name := "maml-jvm",
    libraryDependencies ++= Seq(
      "org.locationtech.geotrellis" %% "geotrellis-raster"    % gtVer,
      "org.locationtech.geotrellis" %% "geotrellis-spark"     % gtVer,
      "org.locationtech.geotrellis" %% "geotrellis-s3"        % gtVer
    )
  ).jsSettings(name := "maml-js")

lazy val mamlJvm = maml.jvm
lazy val mamlJs = maml.js
lazy val mamlSpark = project.in(file("spark"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.locationtech.geotrellis" %% "geotrellis-spark-testkit" % gtVer % "test",
      "org.apache.spark"            %% "spark-core"               % "2.4.0" % "provided",
      "org.apache.spark"            %% "spark-sql"                % "2.4.0" % "provided"
    )
  )
  .dependsOn(mamlJvm)
