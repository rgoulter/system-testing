package systemTestingDSL

import com.typesafe.config.Config
import systemTestingDSL.testSuite.HipTestSuite

class RegressionTestReferenceBuilder(configuration: Config) {

  def getFileList(directory: String): Array[String] = {
    FileSystemUtilities.getRecursiveListOfFilesWithRegex(directory, "*.ss").map(_.getAbsolutePath())
  }

  def buildHipTests() = {
    val files = getFileList(configuration.getString("HIP_REF_SOURCE_DIRECTORY"))
    val outputDirectory = configuration.getString("HIP_REF_OUTPUT_DIRECTORY")
    val suite = new HipTestSuite()
    files.foreach(file => suite.addTest("hip", file, "--print-min", outputDirectory, "", ""))
  }
}
