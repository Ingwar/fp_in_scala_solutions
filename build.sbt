import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.github.Ingwar.fpinscala"
ThisBuild / organizationName := "fpinscala"

lazy val root = (project in file("."))
  .settings(
    name := "fp_in_scala_solutions",
    libraryDependencies += scalaTest % Test,
    ammoniteVersion := "1.6.5"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
