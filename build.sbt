ThisBuild / scalaVersion := "2.13.7"
ThisBuild / organization := "examples.parser"

lazy val json5Parser = (project in file("."))
  .settings(
    name := "json5-scala",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )
