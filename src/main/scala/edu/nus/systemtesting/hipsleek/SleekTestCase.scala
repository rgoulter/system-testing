package edu.nus.systemtesting.hipsleek

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.ProgramFlags.{ flagsOfProgram, isFlag }
import edu.nus.systemtesting.ExecutionOutput
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.ConstructTests

trait ConstructSleekTests extends ConstructTests[SleekTestCase] {
  implicit def constructTestCase(tc : TestCaseBuilder) : SleekTestCase =
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
  def checkResults(expectedOutput : String, output : ExecutionOutput) : Either[List[String], Iterable[(String, String)]] = {
    // Sleek expected output is like
    //   "Fail, Valid, Valid, Fail"
    val expectedOutputList = expectedOutput.split(",").map(_.trim)

    // `parse` is responsible for populating `results` with
    // lines which match `regex`.
    val results = filterLinesMatchingRegex(output.output, regex)

    if (results.isEmpty) {
      val testFlags = arguments.split(" ").filter(isFlag)
      val SleekFlags = flagsOfProgram(commandName)
      val invalidFlags = testFlags.filterNot(SleekFlags.contains)

      if (!invalidFlags.isEmpty) {
        val flagsStr = invalidFlags.map(f => s"Invalid flag $f\n")

        return Left("Binary failed to execute. Please investigate" +: flagsStr.toList)
      } else {
        // Could try searching the output for errors?
        return Left("Binary failed to execute. Please investigate" +:
                    List("Output was:\n" +
                         output.output))
      }
    }

    if (results.size < expectedOutputList.size) {
      return Left(List("TestCase Overspecified! (More expected results than actual)."))
    } else if (results.size > expectedOutputList.size) {
      return Left(List("TestCase Underspecified! (Fewer expected results than actual)."))
    }

    def resultFromOutputLine(resultLine : String) : String = {
      // resultLine is like:
      //   Entail 1: Fail.(may) cause: x!=null & r_45!=x & (((1<=n & r_45!=null) | 
      if (resultLine.contains("Valid"))
        "Valid"
      else
        "Fail"
    }

    val diff = expectedOutputList.zip(results).map({
      case (expected, resultLine) => {
        val actual = resultFromOutputLine(resultLine)

        if (resultLine.contains(expected)) {
          None
        } else {
          Some((expected, actual))
        }
      }
    }).flatten

    return Right(diff)
  }
}
