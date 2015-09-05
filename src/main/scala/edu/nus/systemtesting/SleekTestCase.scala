package edu.nus.systemtesting

import scala.collection.mutable.MutableList

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.output.ConsoleOutputGenerator
import edu.nus.systemtesting.ProgramFlags.{ isFlag, flagsOfProgram }

class SleekTestCaseBuilder(testcase : SleekTestCase = SleekTestCase()) {

  def build() : SleekTestCase = testcase
}

case class SleekTestCase(commandName : String = "",
                         fileName : String = "",
                         arguments : String = "",
                         outputDirectory : String = "",
                         outputFileName : String = "",
                         expectedOutput : String = "",
                         regex : String = "Entail .*:\\s.*(Valid|Fail).*|Entailing lemma .*:\\s.*(Valid|Fail).*")
    extends Runnable with ConsoleOutputGenerator {
  override def formCommand() : String = {
    Seq(commandName, arguments, fileName).mkString(" ")
  }

  def run() = {
    val res@(outp, time) = this.execute

    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectory, outp.output)

    res
  }

  def generateOutput() : TestCaseResult = {
    val (outp, time) = run

    generateTestResult(outp, time)
  }

  // TODO: Either would make a better return type here.
  def checkResults(expectedOutput : String, output : ExecutionOutput) : (Option[String], Boolean) = {
    val expectedOutputList = expectedOutput.split(DEFAULT_TEST_OUTPUT_SEPARATOR).map(_.trim)

    // `parse` is responsible for populating `results` with
    // lines which match `regex`.
    val results = filterLinesMatchingRegex(output.output, regex)
    val filteredResults = results.zipWithIndex

    var resultOutput = ""

    if (filteredResults.isEmpty) {
      val testFlags = arguments.split(" ").filter(isFlag)
      val SleekFlags = flagsOfProgram(commandName)
      val invalidFlags = testFlags.filterNot(SleekFlags.contains)

      if (!invalidFlags.isEmpty) {
        val flagsStr = invalidFlags.map(f => s"Invalid flag $f\n").mkString

        return (Some("Binary failed to execute. Please investigate\n" + flagsStr), false)
      } else {

        return (Some("Binary failed to execute. Please investigate\n" +
                     "Output was:\n" +
                     output.output),
                false)
      }
    }

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

    val resultsIter = filteredResults.iterator
    val expectedIter = expectedOutputList.iterator

    var unmatchedResults = "\nUnmatched:\n"

    for (count <- 0 until minSize) {
      unmatchedResults += had(resultsIter.next)
      unmatchedResults += expected(expectedIter.next)
    }

    if (resultsIter.hasNext)
      unmatchedResults += "\nExtra Sleek Entail Output\n\n"
    while (resultsIter.hasNext)
      unmatchedResults += resultsIter.next + "\n"

    if (expectedIter.hasNext)
      unmatchedResults += "\nExtra Expected Results\n"
    while (expectedIter.hasNext)
      unmatchedResults += expectedIter.next + "\n"

    return (Some(unmatchedResults), false)
  }

  def generateTestResult(output : ExecutionOutput, time : Long) : TestCaseResult = {
    val (err, passed) = checkResults(expectedOutput, output)

    val result = if (passed) TestPassed else TestFailed

    new TestCaseResult(commandName, fileName, arguments, output, time, result, remarks = err.toList)
  }

  //
  // Helper functions for DSL-esque construction of testcase.
  //

  def runCommand(commandName : String) =
    copy(commandName = commandName)

  def onFile(fileName : String) =
    copy(fileName = fileName)

  def withArguments(arguments : String) =
    copy(arguments = arguments)

  def storeOutputInDirectory(outputDirectory : String) =
    copy(outputDirectory = outputDirectory)

  def withOutputFileName(outputFileName : String) =
    copy(outputFileName = outputFileName)

  def checkAgainst(expectedOutput : String) =
    copy(expectedOutput = expectedOutput)

  def usingRegex(regex : String) =
    copy(regex = regex)
}
