name              := "system-testing"

version           := "0.6.0-SNAPSHOT"

organization      := "edu.nus"

scalaVersion      := "2.10.5"

publishMavenStyle := true

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.4" % "test"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.0.4"

libraryDependencies += "joda-time" % "joda-time" % "2.8.2"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

libraryDependencies += "org.antlr" % "ST4" % "4.0.8"

resolvers += Resolver.sonatypeRepo("public")

mainClass in Compile := Some("edu.nus.systemtesting.hipsleek.app.Main")


parallelExecution in Test := false

import com.github.retronym.SbtOneJar._

oneJarSettings

// Tag the slow tests as slow, to save time.
addCommandAlias("testNotSlow", "testOnly -- -l edu.nus.systest.tags.SlowTest")

addCommandAlias("t", "~testNotSlow")
