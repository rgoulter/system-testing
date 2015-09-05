package edu.nus.systemtesting

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.ProgramFlags.flagsOfProgram
import edu.nus.systemtesting.ProgramFlags.isFlag

object SleekTestCase {
  implicit def constructSleekTestCase(tc : TestCaseBuilder) : SleekTestCase =
    new SleekTestCase(tc.commandName,
                      tc.fileName,
                      tc.arguments,
                      tc.outputDirectory,
                      tc.outputFileName,
                      tc.expectedOutput)
}

class SleekTestCase(cmd : String = "",
                    fn : String = "",
                    args : String = "",
                    outDir : String = "",
                    outFN : String = "",
                    expectedOut : String = "",
                    regex : String = "Entail .*:\\s.*(Valid|Fail).*|Entailing lemma .*:\\s.*(Valid|Fail).*")
    extends TestCase(cmd, fn, args, outDir, outFN, expectedOut) {
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
}
