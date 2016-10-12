lazy val commonSettings = Seq(
  organization := "qa",
  version := "0.1",
  scalaVersion := "2.11.8",

  resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",

  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.5",

  updateOptions := updateOptions.value.withCachedResolution(true)
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(core, examples)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "0.12",

      "org.scalatest" %% "scalatest" % "3.0.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.2" % Test
    )
  )

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .dependsOn(core)
