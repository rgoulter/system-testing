package systemTestingDSL

import scala.collection.mutable.MutableList
import java.io.File

class SleekTestCaseBuilder {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""
  var expectedOutput: String = ""
  var regex: String = "Entail\\s.*.*Valid.*|.*Fail.*"

  def runCommand(commandName: String): SleekTestCaseBuilder = {
    this.commandName = commandName
    this
  }

  def onFile(fileName: String): SleekTestCaseBuilder = {
    this.fileName = fileName
    this
  }

  def withArguments(arguments: String): SleekTestCaseBuilder = {
    this.arguments = arguments
    this
  }

  def storeOutputInDirectory(outputDirectory: String): SleekTestCaseBuilder = {
    this.outputDirectory = outputDirectory
    this
  }

  def withOutputFileName(outputFileName: String): SleekTestCaseBuilder = {
    this.outputFileName = outputFileName
    this
  }

  def checkAgainst(expectedOutput: String): SleekTestCaseBuilder = {
    this.expectedOutput = expectedOutput
    this
  }

  def usingRegex(regex: String): SleekTestCaseBuilder = {
    this.regex = regex
    this
  }

  def build: SleekTestCase = new SleekTestCase(this)
}
class SleekTestCase(builder: SleekTestCaseBuilder)
  extends Runnable with Parser {
  var commandName = builder.commandName
  var fileName = builder.fileName
  var arguments = builder.arguments
  var outputFileName = builder.outputFileName
  var expectedOutput = builder.expectedOutput
  var outputDirectory = builder.outputDirectory
  var regex = builder.regex

  var results: MutableList[String] = MutableList()
  def process(source: String, rule: String): Unit = {
    results += rule
  }

  def run() = {
    val output = this.execute
    if (outputFileName.length > 0)
      generateOutputFile(output)
    output
  }

  def printResults() = {
    for(result <- results)
      println(result + ", ")
    print("end of results")
  }

  def generateOutput() = {
    this.parse(run, builder.regex, NEW_LINE)
//    printResults()
    generateTestResults()
  }

  def checkResults(): Boolean = {
    val expectedOutputList: Array[String] = expectedOutput.split(DEFAULT_TEST_OUTPUT_SEPARATOR)
//    println("Expect result size: " + expectedOutputList.size)
//    println("Actual result size: " + results.size)
    if (expectedOutputList.size != results.size)
      return false
    for ((x, i) <- results.view.zipWithIndex)
      if (!x.contains(expectedOutputList(i)))
        return false
    return true
  }

  def generateTestResults(): String = if (checkResults()) "Passed" else "Failed"

  def generateOutputFile(consoleOutput: String) = {
    fileSystemUtilities.printToFile(new File(outputDirectory.concat(File.separator).concat(outputFileName)))(p => p.print(consoleOutput))
  }
}
