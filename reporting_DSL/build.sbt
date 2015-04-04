name              := "ReportingDSL"

version           := "0.1.0-SNAPSHOT"

organization      := "com.rohitmukherjee"

scalaVersion      := "2.10.3"

publishMavenStyle := true

libraryDependencies ++= Seq(
   "junit" % "junit" % "4.8.1" % "test"
   )

resolvers += Classpaths.typesafeResolver

scalariformSettings
