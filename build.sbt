lazy val commonSettings = Seq(
  organization := "qa",
  version := "0.1",
  scalaVersion := "2.11.8",

  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(core, examples)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.0" % Test,
      "org.mockito" % "mockito-all" % "1.10.19" % Test
    )
  )

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .dependsOn(core)
