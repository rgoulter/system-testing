package systemTestingDSL

import com.typesafe.config.Config
import systemTestingDSL.testSuite.HipTestSuite
import scala.collection.mutable.ArrayBuffer

class RegressionTestReferenceBuilder(configuration: Config) {

  private def getFileList(directory: String): Array[String] = {
    FileSystemUtilities.getRecursiveListOfFilesWithRegex(directory, "*.ss").map(_.getAbsolutePath())
  }

  def buildHipTests(): ArrayBuffer[GenericTestCase] = {
    val files = getFileList(configuration.getString("HIP_REF_SOURCE_DIRECTORY"))
    val outputDirectory = configuration.getString("HIP_REF_OUTPUT_DIRECTORY")
    val referenceRuns = ArrayBuffer[GenericTestCase]()
    files.foreach(file => referenceRuns += new GenericTestCase("hip", file, "", outputDirectory, file.concat(".out")))
    referenceRuns
  }

  def run(): Unit = {
    val references = this.buildHipTests()
    references.foreach(_.run)
  }
}
