addDependencyTreePlugin

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("com.47deg" % "sbt-microsites" % "1.4.4")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.16.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
