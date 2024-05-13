import Dependencies._

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

val commonSettings = Seq(
  scalaVersion := "2.12.19",
  crossScalaVersions := Seq("2.12.19", "2.13.14"),
  resolvers ++= Resolver.sonatypeOssRepos("releases") ++ Resolver.sonatypeOssRepos("snapshots") ++ Seq(
    "locationtech-releases".at("https://repo.locationtech.org/content/groups/releases"),
    "locationtech-snapshots".at("https://repo.locationtech.org/content/groups/snapshots")
  ),
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
    "-Ypatmat-exhaust-depth",
    "100"
  ),
  libraryDependencies ++= (
    if (priorTo213(scalaVersion.value)) Seq(compilerPlugin(("org.scalamacros" %% "paradise" % "2.1.1").cross(CrossVersion.full)))
    else Nil
  ),
  scalacOptions ++= (if (priorTo213(scalaVersion.value)) Nil else Seq("-Ymacro-annotations"))
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
  Test / publishArtifact := false
) ++ sonatypeSettings

lazy val sonatypeSettings = Seq(
  publishMavenStyle := true,
  sonatypeProfileName := "com.azavea",
  developers := List(
    Developer("moradology", "Nathan Zimmerman", "nzimmerman@azavea.com", url("https://github.com/moradology")),
    Developer("echeipesh", "Eugene Cheipesh", "echeipesh@azavea.com", url("https://github.com/echeipesh")),
    Developer("lossyrob", "Rob Emanuele", "remanuele@azavea.com", url("https://github.com/lossyrob"))
  ),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  publishTo := sonatypePublishTo.value
)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(publishSettings) // these settings are needed to release all aggregated modules under this root module
  .settings(noPublishSettings) // this is to exclue the root module itself from being published
  .aggregate(mamlJs, mamlJvm, mamlSpark)
  .enablePlugins(ScalaJSPlugin)

lazy val maml = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      scalacheck % Test,
      scalatest % Test,
      logging % Test,
      cats("core").value,
      cats("effect").value,
      circe("core").value,
      circe("generic").value,
      circe("generic-extras").value,
      circe("parser").value,
      circe("optics").value
    )
  )
  .jvmSettings(
    name := "maml-jvm",
    libraryDependencies ++= Seq(
      geotrellis("raster").value,
      geotrellis("layer").value,
      geotrellis("proj4").value
    )
  )
  .jsSettings(
    name := "maml-js",
    libraryDependencies += geotrellis("raster").value
  )

lazy val mamlJvm = maml.jvm
lazy val mamlJs = maml.js
lazy val mamlSpark = project
  .in(file("spark"))
  .dependsOn(mamlJvm)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(name := "maml-spark")
  .settings(
    libraryDependencies ++= Seq(
      spark("sql").value % Test,
      spark("core").value % Provided,
      geotrellis("spark-testkit").value % Test,
      geotrellis("spark").value % Provided
    )
  )
  .settings(
    Test / fork := false,
    Test / parallelExecution := false
  )

def priorTo213(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }
