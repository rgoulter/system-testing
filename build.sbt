name              := "system-testing"

version           := "0.3.0-SNAPSHOT"

organization      := "edu.nus"

scalaVersion      := "2.10.5"

publishMavenStyle := true

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.4" % "test"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.0.4"
