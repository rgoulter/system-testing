package systemTestingDSL

import scala.collection.mutable.MutableList
import java.io.File
import scala.collection.mutable.ArrayBuffer
import systemTestingDSL.outputGenerator.HTMLOutputGenerator

class SleekTestCaseBuilder {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""
  var expectedOutput: String = ""
  var regex: String = "Entail\\s\\d\\d:\\s.*Valid.*|Entail\\s\\d\\d:\\s.*Fail.*"

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
  extends Runnable with Parser with InferenceTester with HTMLOutputGenerator {
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
      writeToFile(this.outputFileName, this.outputDirectory, output)
  }

  def generateOutput(): (Option[String], String) = {
    run
    this.parse(this.output, builder.regex, NEW_LINE)
    generateTestResult
  }

  def testInference(results: ArrayBuffer[(String, String)]) = {
    checkCorpus(this.output, results)
  }

  def checkResults(): (Option[String], Boolean) = {
    val expectedOutputList: Array[String] = expectedOutput.split(DEFAULT_TEST_OUTPUT_SEPARATOR)
    var resultOutput = ""
    val filteredResults = results.view.filter(_.contains("Entail")).zipWithIndex
    if (filteredResults.size != expectedOutputList.size)
      matchUnequalFailedTests(results, expectedOutputList)
    for ((result, i) <- filteredResults)
      if (!result.contains(expectedOutputList(i))) {
        resultOutput += had(result)
        resultOutput += expected(expectedOutputList(i))
        return (Some(resultOutput), false)
      }
    return (None, true)
  }

  def matchUnequalFailedTests(filteredResults: Seq[String], expectedOutputList: Seq[String]): (Option[String], Boolean) = {
    val minSize = Math.min(filteredResults.length, expectedOutputList.size)
    var count, i = 0
    val unmatchedResults = new ArrayBuffer[String]
    for (count <- 0 until minSize) {
      if (!filteredResults(count).contains(expectedOutputList(count)))
        unmatchedResults += had(filteredResults(count))
      unmatchedResults += expected(expectedOutputList(count))
    }
    unmatchedResults += "\nUnmatched\n"
    for (i <- count until filteredResults.length)
      unmatchedResults += filteredResults(i)
    for (i <- count until expectedOutputList.length)
      unmatchedResults += expectedOutputList(i)
    return (None, false)
  }
  def generateTestResult(): (Option[String], String) = {
    val results = checkResults()
    if (results._2)
      (None, "Passed")
    else (results._1, "Failed")
  }
}
