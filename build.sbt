import xerial.sbt.Sonatype._

import Dependencies._

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
  scalaVersion := "2.12.10",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    "eclipse-releases" at "https://repo.eclipse.org/content/groups/releases",
    "eclipse-snapshots" at "https://repo.eclipse.org/content/groups/snapshots"
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

lazy val maml = crossProject.in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      scalacheck % Test,
      scalatest % Test,
      logging % Test,
      droste,
      cats("core").value,
      cats("effect").value,
      circe("core").value,
      circe("generic").value,
      circe("generic-extras").value,
      circe("parser").value,
      circe("optics").value
    )
  ).jvmSettings(
    name := "maml-jvm",
    libraryDependencies ++= Seq(
      geotrellis("raster").value,
      geotrellis("layer").value,
      geotrellis("proj4").value
    )
  ).jsSettings(
    name := "maml-js",
    libraryDependencies += geotrellis("raster").value
)

lazy val mamlJvm = maml.jvm
lazy val mamlJs = maml.js
lazy val mamlSpark = project.in(file("spark"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      spark("sql").value % Test,
      spark("core").value % Provided,
      geotrellis("spark-testkit").value % Test,
      geotrellis("spark").value % Provided
    )
  ).dependsOn(mamlJvm)