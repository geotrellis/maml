name := "MAML"

scalaVersion in ThisBuild := "2.11.8"

lazy val root = project.in(file(".")).
  aggregate(mamlJS, mamlJVM).
  settings(
    publish := {},
    publishLocal := {}
  ).enablePlugins(ScalaJSPlugin)

lazy val maml = crossProject.in(file(".")).
  settings(
    name := "MAML",
    version := "0.1-SNAPSHOT",
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck"           % "1.13.4" % "test",
      "org.scalatest"  %% "scalatest"            % "3.0.1"  % "test",
      "org.typelevel"  %% "cats"                 % "0.9.0",
      "io.circe"      %%% "circe-core"           % "0.8.0",
      "io.circe"      %%% "circe-generic"        % "0.8.0",
      "io.circe"      %%% "circe-generic-extras" % "0.8.0",
      "io.circe"      %%% "circe-parser"         % "0.8.0",
      "io.circe"      %%% "circe-optics"         % "0.8.0"
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.locationtech.geotrellis" %% "geotrellis-raster" % "1.1.1"
    )
  )
  .jsSettings()

lazy val mamlJVM = maml.jvm
lazy val mamlJS = maml.js


