package edu.nus.systemtesting.hipsleek

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.ProgramFlags.{ flagsOfProgram, isFlag }
import edu.nus.systemtesting.ExecutionOutput
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.Result
import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.ConstructTests
import java.nio.file.Path
import java.nio.file.Paths

trait ConstructSleekTests extends ConstructTests[SleekTestCase] {
  implicit def constructTestCase(tc: TestCaseBuilder): SleekTestCase =
    new SleekTestCase(tc.projectDir,
                      tc.commandName,
                      tc.corpusDir,
                      tc.fileName,
                      tc.arguments,
                      tc.expectedOutput,
                      tc.timeout)
}

class SleekTestCase(projDir: Path = Paths.get(""),
                    cmd: Path = Paths.get(""),
                    corpDir: Path = Paths.get(""),
                    fn: Path = Paths.get(""),
                    args: String = "",
                    expectedOut: String = "",
                    timeout: Int = 300,
                    regex: String = "Entail .*:\\s.*(Valid|Fail).*|Entailing lemma .*:\\s.*(Valid|Fail).*")
    extends TestCase(projDir, cmd, corpDir, fn, args, expectedOut, timeout) {
  def checkResults(expectedOutput: String, output: ExecutionOutput): Either[List[String], Iterable[Result]] = {
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
        val flagsStr = invalidFlags.map(f => s"Invalid flag $f").mkString("\n")

        return Left(List("Binary failed to execute. Please investigate", flagsStr))
      } else {
        // Could try searching the output for errors?
        return Left("Binary failed to execute. Please investigate" +:
                    List("Output was:\n" +
                         output.output.trim))
      }
    }

    if (results.size < expectedOutputList.size) {
      return Left(List("TestCase Overspecified! (More expected results than actual)."))
    } else if (results.size > expectedOutputList.size) {
      return Left(List("TestCase Underspecified! (Fewer expected results than actual)."))
    }

    def resultFromOutputLine(resultLine: String): String = {
      // resultLine is like:
      //   Entail 1: Fail.(may) cause: x!=null & r_45!=x & (((1<=n & r_45!=null) | 
      if (resultLine.contains("Valid"))
        "Valid"
      else
        "Fail"
    }

    val resultUnits = expectedOutputList.zip(results).zipWithIndex.map({
      case ((expected, resultLine), idx) => {
        val actual = resultFromOutputLine(resultLine)

        Some(Result(idx.toString, expected, actual))
      }
    }).flatten

    return Right(resultUnits)
  }
}
