name := "MAML"

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
      "-Ypartial-unification",
      "-Ypatmat-exhaust-depth", "100"
    ),
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
  .jvmSettings(
    resolvers += Resolver.bintrayRepo("hseeberger", "maven"),
    libraryDependencies ++= Seq(
      "org.locationtech.geotrellis" %% "geotrellis-raster"    % "1.1.1",
      "org.locationtech.geotrellis" %% "geotrellis-spark"     % "1.1.1",
      "org.locationtech.geotrellis" %% "geotrellis-s3"        % "1.1.1",
      "org.apache.spark"            %% "spark-core"           % "2.0.0",
      "com.typesafe.akka"           %% "akka-actor"           % "2.5.3",
      "com.typesafe.akka"           %% "akka-stream"          % "2.5.3",
      "com.typesafe.akka"           %% "akka-testkit"         % "2.5.3",
      "com.typesafe.akka"           %% "akka-http"            % "10.0.9",
      "com.typesafe.akka"           %% "akka-http-spray-json" % "10.0.9",
      "com.typesafe.akka"           %% "akka-http-testkit"    % "10.0.9",
      "de.heikoseeberger"           %% "akka-http-circe"      % "1.17.0"

    )
  )
  .jsSettings()

lazy val mamlJVM = maml.jvm
lazy val mamlJS = maml.js

/* Microsite Settings */

/* To generate the microsite locally, use `sbt makeMicrosite`.
 * To publish the site to Github, use `sbt publishMicrosite`.
 *
 * Spark deps must not be marked `provided` while doing these, or you will get errors.
 */
enablePlugins(MicrositesPlugin)
enablePlugins(SiteScaladocPlugin)

micrositeName := "MAML"
micrositeDescription := "Map Algebra Model Lab"
micrositeAuthor := "GeoTrellis Team at Azavea"
micrositeGitterChannel := false
micrositeOrganizationHomepage := "https://www.azavea.com/"
micrositeGithubOwner := "geotrellis"
micrositeGithubRepo := "maml"
micrositeBaseUrl := "/maml"
micrositeDocumentationUrl := "/maml/latest/api" /* Location of Scaladocs */
