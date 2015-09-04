name              := "system-testing"

version           := "0.2.0-SNAPSHOT"

organization      := "edu.nus"

scalaVersion      := "2.10.3"

publishMavenStyle := true

libraryDependencies ++= Seq(
   "junit" % "junit" % "4.8.1" % "test"
)

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.3"
