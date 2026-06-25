ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "simulador_polarizacion",

    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.21",
      "org.plotly-scala" %% "plotly-render" % "0.8.4",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    ),

    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
    ),

    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false
  )