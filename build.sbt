import com.scalapenos.sbt.prompt.SbtPrompt.autoImport._

promptTheme := com.scalapenos.sbt.prompt.PromptThemes.ScalapenosTheme

val mamlVersion = "0.0.15" + scala.util.Properties.envOrElse("MAML_VERSION_SUFFIX", "")

/** Project configurations */
lazy val root = project.in(file("."))
  .aggregate(mamlJs, mamlJvm, mamlSpark)
  .settings(commonSettings:_*)
  .settings(
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
  ).enablePlugins(ScalaJSPlugin)

val circeVer = "0.10.0-M1"
val gtVer    = "2.0.0-RC2"

lazy val maml = crossProject.in(file("."))
  .settings(publishSettings:_*)
  .settings(commonSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck"             %% "scalacheck"           % "1.13.4" % "test",
      "org.scalatest"              %% "scalatest"            % "3.0.1"  % "test",
      "com.typesafe.scala-logging" %% "scala-logging"        % "3.9.0",
      "org.typelevel"              %% "cats-effect"          % "0.10.1",
      "io.circe"                   %% "circe-core"           % circeVer,
      "io.circe"                   %% "circe-generic"        % circeVer,
      "io.circe"                   %% "circe-generic-extras" % circeVer,
      "io.circe"                   %% "circe-parser"         % circeVer,
      "io.circe"                   %% "circe-optics"         % circeVer
    )
  ).jvmSettings(
    name := "maml-jvm",
    libraryDependencies ++= Seq(
      "org.locationtech.geotrellis" %% "geotrellis-raster"    % gtVer,
      "org.locationtech.geotrellis" %% "geotrellis-spark"     % gtVer,
      "org.locationtech.geotrellis" %% "geotrellis-s3"        % gtVer,
      "org.apache.spark"            %% "spark-core"           % "2.2.0"
    )
  ).jsSettings(name := "maml-js")

lazy val mamlJvm = maml.jvm
lazy val mamlJs = maml.js
lazy val mamlSpark = project.in(file("spark"))
  .dependsOn(mamlJvm)
  .settings(
    libraryDependencies ++= Seq(
      "org.locationtech.geotrellis" %% "geotrellis-spark-testkit" % gtVer % "test"
    )
  ).settings(publishSettings:_*)
  .settings(commonSettings:_*)


/** Common settings */
lazy val publishSettings =
  Seq(
    bintrayOrganization := Some("azavea"),
    bintrayRepository := "maven",
    bintrayVcsUrl := Some("https://github.com/geotrellis/maml.git"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    homepage := Some(url("https://geotrellis.github.io/maml"))
  )

val commonSettings = Seq(
  organization := "com.azavea",
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  version := mamlVersion,
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12", "2.12.3"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    "locationtech-releases" at "https://repo.locationtech.org/content/groups/releases"
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
