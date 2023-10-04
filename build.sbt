ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"

lazy val root = (project in file("."))
  .settings(
    name := "cats"
  )
