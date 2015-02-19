package systemTestingDSL

import scala.collection.mutable.MutableList
import java.io.File
import scala.collection.mutable.ArrayBuffer

class HipTestCaseBuilder {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""
  var expectedOutput: String = ""
  var regex: String = "Entail\\s\\d\\d:\\s.*Valid.*|Entail\\s\\d\\d:\\s.*Fail.*"

  def runCommand(commandName: String): HipTestCaseBuilder = {
    this.commandName = commandName
    this
  }

  def onFile(fileName: String): HipTestCaseBuilder = {
    this.fileName = fileName
    this
  }

  def withArguments(arguments: String): HipTestCaseBuilder = {
    this.arguments = arguments
    this
  }

  def storeOutputInDirectory(outputDirectory: String): HipTestCaseBuilder = {
    this.outputDirectory = outputDirectory
    this
  }

  def withOutputFileName(outputFileName: String): HipTestCaseBuilder = {
    this.outputFileName = outputFileName
    this
  }

  def checkAgainst(expectedOutput: String): HipTestCaseBuilder = {
    this.expectedOutput = expectedOutput
    this
  }

  def usingRegex(regex: String): HipTestCaseBuilder = {
    this.regex = regex
    this
  }

  def build: HipTestCase = new HipTestCase(this)
}
class HipTestCase(builder: HipTestCaseBuilder)
  extends Runnable with Parser with InferenceTester {
  var commandName = builder.commandName
  var fileName = builder.fileName
  var arguments = builder.arguments
  var outputFileName = builder.outputFileName
  var expectedOutput = builder.expectedOutput
  var outputDirectory = builder.outputDirectory
  var regex = builder.regex
  var output: String = ""

  var results: MutableList[String] = MutableList()
  def process(source: String, rule: String): Unit = {
    results += rule
  }

  def run() = {
    this.output = this.execute
    if (outputFileName.length > 0)
      generateOutputFile(output)
  }

  def printResults() = {
    for (result <- results)
      println(result + ", ")
    print("end of results")
  }

  def generateOutput() = {
    run
    this.parse(this.output, builder.regex, NEW_LINE)
    generateTestResults()
  }

  def testInference(results: ArrayBuffer[(String, String)]) = {
    checkCorpus(this.output, results)
  }

  def checkResults(): Boolean = {
    val expectedOutputList: Array[String] = expectedOutput.split(DEFAULT_TEST_OUTPUT_SEPARATOR)
    val filteredResults = results.view.filter(line => line.contains("Entail")).zipWithIndex
    if (filteredResults.size != expectedOutputList.size)
      return false
    for ((x, i) <- filteredResults)
      if (!x.contains(expectedOutputList(i))) {
        println("Had: " + x)
        println("Expected: " + expectedOutputList(i))
        return false
      }
    return true
  }

  def generateTestResults(): String = if (checkResults()) "Passed" else "Failed"

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