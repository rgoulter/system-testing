name              := "system-testing"

version           := "0.3.0-SNAPSHOT"

organization      := "edu.nus"

scalaVersion      := "2.10.5"

publishMavenStyle := true

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.4" % "test"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.0.4"

libraryDependencies += "joda-time" % "joda-time" % "2.8.2"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

resolvers += Resolver.sonatypeRepo("public")

mainClass in Compile := Some("edu.nus.systemtesting.hipsleek.Main")


import com.github.retronym.SbtOneJar._

oneJarSettings
