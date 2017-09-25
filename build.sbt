/** Project configurations */
lazy val root = project.in(file("."))
  .aggregate(mamlJS, mamlJVM, mamlRDD)
  .enablePlugins(ScalaJSPlugin)

lazy val maml = crossProject.in(file("."))
  .settings(publishSettings:_*)
  .settings(commonSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck"              % "1.13.4" % "test",
      "org.scalatest"  %% "scalatest"               % "3.0.1"  % "test",
      "org.typelevel"  %% "cats"                    % "0.9.0",
      "io.circe"      %%% "circe-core"              % "0.8.0",
      "io.circe"      %%% "circe-generic"           % "0.8.0",
      "io.circe"      %%% "circe-generic-extras"    % "0.8.0",
      "io.circe"      %%% "circe-parser"            % "0.8.0",
      "io.circe"      %%% "circe-optics"            % "0.8.0"
    )
  )

lazy val mamlJVM = maml.jvm
lazy val mamlJS = maml.js
lazy val mamlRDD = project.in(file("rdd"))
  .dependsOn(mamlJVM)
  .settings(publishSettings:_*)
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
  licenses ++= Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))),
  version := "0.0.1",
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.11.11", "2.12.1"),
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

