val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "DayFourteen",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"
  )
