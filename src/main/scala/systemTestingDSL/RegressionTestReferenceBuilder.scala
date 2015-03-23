package systemTestingDSL

import com.typesafe.config.Config
import scala.collection.mutable.ArrayBuffer
import collection.JavaConversions._

class RegressionTestReferenceBuilder(configuration: Config) {

  private def getFileList(directory: String, extension: String): Array[String] = {
    FileSystemUtilities.getRecursiveListOfFilesWithRegex(directory, extension).map(_.getAbsolutePath())
  }

  def buildTests(): ArrayBuffer[GenericTestCase] = {
    val refTests = configuration.getConfigList("REFERENCE_TESTS")
    println(refTests)
    val referenceRuns = ArrayBuffer[GenericTestCase]()
    for (configuration <- refTests) {
      val files = getFileList(configuration.getString("SOURCE_DIRECTORY"), configuration.getString("EXTENSION"))
      val outputDirectory = configuration.getString("REF_OUTPUT_DIRECTORY")
      val commandName = configuration.getString("COMMAND_NAME")
      val arguments = configuration.getString("ARGUMENTS")
      files.foreach(file => referenceRuns += new GenericTestCase(commandName, file, "", outputDirectory, file.substring(file.lastIndexOf("/") + 1)))
    }
    referenceRuns
  }

  def run(): Unit = {
    val references = this.buildTests()
        references.foreach(_.run)
  }
}
