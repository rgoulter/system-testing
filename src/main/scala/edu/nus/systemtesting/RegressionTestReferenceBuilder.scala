package edu.nus.systemtesting

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.ArrayBuffer

import com.typesafe.config.Config

class RegressionTestReferenceBuilder(configuration: Config) extends GetFileList {
  def buildTests(): ArrayBuffer[GenericTestCase] = {
    val refTests = configuration.getConfigList("BUILD_REFERENCE_TESTS")
    val referenceRuns = ArrayBuffer[GenericTestCase]()

    for (configuration <- refTests) {
      val files = getFileList(configuration.getString("SOURCE_DIRECTORY"),
        configuration.getString("SOURCE_EXTENSION"))
      println(files)

      val outputDirectory = configuration.getString("REF_OUTPUT_DIRECTORY")
      val commandName = configuration.getString("COMMAND_NAME")
      val arguments = configuration.getString("ARGUMENTS")
      val referenceFileExtension = configuration.getString("REF_EXTENSION")

      files.foreach(file => {
        val fileBasename = file.substring(file.lastIndexOf("/") + 1)
        referenceRuns +=
          new GenericTestCase(commandName, file, arguments, outputDirectory, fileBasename, referenceFileExtension)
      })
    }

    referenceRuns
  }

  def run(): Unit = {
    val references = this.buildTests()

    references.foreach(_.run)
  }
}
