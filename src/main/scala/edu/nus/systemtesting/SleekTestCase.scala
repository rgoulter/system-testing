package edu.nus.systemtesting

import scala.collection.mutable.MutableList

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.output.ConsoleOutputGenerator

class SleekTestCaseBuilder() {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""
  var expectedOutput: String = ""
  // Regex with entailing lemma:
  // "Entail.*:\\s.*Valid.*|Entail.*:\\s.*Fail.*|Entailing lemma.*:.*Valid.*|Entailing lemma.*:.*Fail.*"
  var regex: String = "Entail \\d+:\\s.*(Valid|Fail).*"

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
    extends Runnable with ConsoleOutputGenerator {
  val commandName = builder.commandName
  val fileName = builder.fileName
  val arguments = builder.arguments
  val outputFileName = builder.outputFileName
  val expectedOutput = builder.expectedOutput
  val outputDirectory = builder.outputDirectory
  val regex = builder.regex

  override def formCommand(): String = {
    Seq(commandName, arguments, fileName).mkString(" ")
  }

  def run() = {
    val res@(outp, time) = this.execute

    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectory, outp.output)

    res
  }

  def generateOutput(): TestCaseResult = {
    val (outp, time) = run

    generateTestResult(outp, time)
  }

  // TODO: Either would make a better return type here.
  def checkResults(expectedOutput: String, output : ExecutionOutput): (Option[String], Boolean) = {
    val expectedOutputList: Array[String] = expectedOutput.split(DEFAULT_TEST_OUTPUT_SEPARATOR)

    // `parse` is responsible for populating `results` with
    // lines which match `builder.regex`.
    val results = filterLinesMatchingRegex(output.output, regex)
    val filteredResults = results.zipWithIndex

    var resultOutput = ""

    if (filteredResults.isEmpty)
      return (Some("Binary failed to execute. Please investigate \n"), false)

    if (filteredResults.size != expectedOutputList.size)
      return matchUnequalFailedTests(results, expectedOutputList)

    for ((result, i) <- filteredResults)
      if (!result.contains(expectedOutputList(i))) {
        resultOutput += had(result)
        resultOutput += expected(expectedOutputList(i))

        return (Some(resultOutput), false)
      }

    return (None, true)
  }

  private def matchUnequalFailedTests(filteredResults : Seq[String],
                                      expectedOutputList : Seq[String]) : (Option[String], Boolean) = {
    val minSize = Math.min(filteredResults.length, expectedOutputList.size)

    var count, i = 0
    var unmatchedResults = ""

    for (count <- 0 until minSize) {
      if (!filteredResults(count).contains(expectedOutputList(count)))
        unmatchedResults += had(filteredResults(count))

      unmatchedResults += expected(expectedOutputList(count))
    }

    unmatchedResults += "\nUnmatched\n"
    unmatchedResults += "\nExtra Sleek Entail Output\n\n"

    for (i <- count until filteredResults.length)
      unmatchedResults += filteredResults(i)

    unmatchedResults += "\nExtra Results\n"

    for (i <- count until expectedOutputList.length)
      unmatchedResults += expectedOutputList(i)

    return (Some(unmatchedResults), false)
  }

  def generateTestResult(output : ExecutionOutput, time : Long) : TestCaseResult = {
    val (err, passed) = checkResults(expectedOutput, output)

    val result = if (passed) TestPassed else TestFailed

    new TestCaseResult(commandName, fileName, arguments, output, time, result, remarks = err.toList)
  }
}
