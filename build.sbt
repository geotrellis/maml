import com.scalapenos.sbt.prompt.SbtPrompt.autoImport._

promptTheme := com.scalapenos.sbt.prompt.PromptThemes.ScalapenosTheme

val mamlVersion = "0.0.5" + scala.util.Properties.envOrElse("MAML_VERSION_SUFFIX", "")

/** Project configurations */
lazy val root = project.in(file("."))
  .aggregate(mamlJs, mamlJvm, mamlSpark)
  .settings(commonSettings:_*)
  .settings(
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
  ).enablePlugins(ScalaJSPlugin)

lazy val maml = crossProject.in(file("."))
  .settings(publishSettings:_*)
  .settings(commonSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck"              % "1.13.4" % "test",
      "org.scalatest"  %% "scalatest"               % "3.0.1"  % "test",
      "org.typelevel"  %% "cats-core"               % "1.0.1",
      "io.circe"      %%% "circe-core"              % "0.9.1",
      "io.circe"      %%% "circe-generic"           % "0.9.1",
      "io.circe"      %%% "circe-generic-extras"    % "0.9.1",
      "io.circe"      %%% "circe-parser"            % "0.9.1",
      "io.circe"      %%% "circe-optics"            % "0.9.1"
    )
  ).jvmSettings(
    name := "maml-jvm",
    libraryDependencies ++= Seq(
      "org.locationtech.geotrellis" %% "geotrellis-raster"    % "2.0.0-RC2",
      "org.locationtech.geotrellis" %% "geotrellis-spark"     % "2.0.0-RC2",
      "org.locationtech.geotrellis" %% "geotrellis-s3"        % "2.0.0-RC2",
      "org.apache.spark"            %% "spark-core"           % "2.2.0" % "provided"
    )
  ).jvmSettings(commonSettings:_*)
  .jsSettings(
    name := "maml-js"
  ).jsSettings(commonSettings:_*)

lazy val mamlJvm = maml.jvm
lazy val mamlJs = maml.js
lazy val mamlSpark = project.in(file("spark"))
  .dependsOn(mamlJvm)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.spark" %% "spark-core" % "2.2.0" % "provided"
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
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.3"),
  resolvers += Resolver.sonatypeRepo("releases"),
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
