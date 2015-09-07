name              := "system-testing"

version           := "0.2.0-SNAPSHOT"

organization      := "edu.nus"

scalaVersion      := "2.10.3"

publishMavenStyle := true

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.4" % "test"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.3"
