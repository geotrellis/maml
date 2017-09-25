name := "mamlJVM"

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
