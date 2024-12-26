//ThisBuild / version := "0.1.0-SNAPSHOT"
//
//ThisBuild / scalaVersion := "3.3.4"
//
//lazy val root = (project in file("."))
//  .settings(
//    name := "Scala_Exercises"
//  )
//
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

name := "Scala_Exercises"
version := "0.1"
scalaVersion := "3.6.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)