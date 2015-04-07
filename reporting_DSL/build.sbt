name              := "ReportingDSL"

version           := "0.1.0-SNAPSHOT"

organization      := "com.rohitmukherjee"

scalaVersion      := "2.10.3"

publishMavenStyle := true

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.11" % Test,
    "com.novocode" % "junit-interface" % "0.11" % Test
      exclude("junit", "junit-dep")
 )

resolvers += Classpaths.typesafeResolver

scalariformSettings
