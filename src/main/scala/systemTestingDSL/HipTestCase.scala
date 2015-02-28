package systemTestingDSL

import scala.collection.mutable.MutableList
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class HipTestCaseBuilder {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""
  var expectedOutput: String = ""
  var regex: String = "Procedure.*FAIL.*|Procedure.*SUCCESS.*"

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

  def buildExpectedOutputMap(results: String): HashMap[String, String] = {
    val outputMap = new HashMap[String, String]
    results.split(",").foreach(result => outputMap.put(result.substring(0, result.indexOf(":")).trim, result.substring(result.indexOf(":") + 1).trim))
    outputMap
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

  def checkResults(): Boolean = {
    val expectedOutputMap = buildExpectedOutputMap(expectedOutput)
    val filteredResults = results.view.filter(_.matches(this.regex))
    if (filteredResults.size != expectedOutputMap.size)
      return false
    for (outputLine <- filteredResults) {
      var methodName = outputLine.split(" ")(1)
      methodName = methodName.substring(0, methodName.indexOf("$"))
      val result: String = if (outputLine.contains("FAIL"))
        "FAIL"
      else "SUCCESS"
      if (!expectedOutputMap(methodName).equals(result))
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