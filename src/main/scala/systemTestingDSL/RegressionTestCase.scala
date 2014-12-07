package systemTestingDSL

import scala.collection.mutable.MutableList
import java.io.File
import systemTestingDSL.matchers._

class RegressionTestCaseBuilder {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""

  def runCommand(commandName: String): RegressionTestCaseBuilder = {
    this.commandName = commandName
    this
  }

  def onFile(fileName: String): RegressionTestCaseBuilder = {
    this.fileName = fileName
    this
  }

  def withArguments(arguments: String): RegressionTestCaseBuilder = {
    this.arguments = arguments
    this
  }

  def storeOutputInDirectory(outputDirectory: String): RegressionTestCaseBuilder = {
    this.outputDirectory = outputDirectory
    this
  }

  def withOutputFileName(outputFileName: String): RegressionTestCaseBuilder = {
    this.outputFileName = outputFileName
    this
  }

  def build: RegressionTestCase = new RegressionTestCase(this)

}

class RegressionTestCase(builder: RegressionTestCaseBuilder)
  extends Runnable {
  var commandName = builder.commandName
  var fileName = builder.fileName
  var arguments = builder.arguments
  var outputFileName = builder.outputFileName
  var outputDirectory = builder.outputDirectory

  var results: MutableList[String] = MutableList()
  def process(source: String, rule: String): Unit = {
    results += rule
  }

  def generateOutput(): Boolean = {
    val fullyQualifiedOutputFileName = outputDirectory.concat(File.separator).concat(outputFileName)
    val outputFileHandle = new File(fullyQualifiedOutputFileName)
    val output = this.execute
    if (!outputFileHandle.exists()) {
      generateOutputFile(output)
      return true
    }
    val outputToDiffMatcher = new OutputToDiffMatcher(output, fullyQualifiedOutputFileName)
    return outputToDiffMatcher.matches
  }

  def checkOutputDirectory = {
    val outputDirectory = new File(this.outputDirectory)
    if (!outputDirectory.exists())
      fileSystemUtilities.createDirectory(this.outputDirectory)
  }

  def generateOutputFile(consoleOutput: String) = {
    checkOutputDirectory
    fileSystemUtilities.printToFile(new File(outputDirectory.concat(File.separator).concat(outputFileName)))(p => p.print(consoleOutput))
  }
}