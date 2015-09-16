package edu.nus.systemtesting.hipsleek

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.ProgramFlags.{ isFlag, flagsOfProgram }
import edu.nus.systemtesting.ExecutionOutput
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.Result
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.ConstructTests
import scala.Left
import scala.Right
import java.nio.file.Path
import java.nio.file.Paths

trait ConstructHipTests extends ConstructTests[HipTestCase] {
  implicit def constructTestCase(tc: TestCaseBuilder): HipTestCase =
    new HipTestCase(tc.commandName,
                    tc.fileName,
                    tc.arguments,
                    tc.expectedOutput,
                    tc.timeout)
}

class HipTestCase(cmd: Path = Paths.get(""),
                  fn: Path = Paths.get(""),
                  args: String = "",
                  expectedOut: String = "",
                  timeout: Int = 300,
                  regex: String = "Procedure.*FAIL.*|Procedure.*SUCCESS.*")
    extends TestCase(cmd, fn, args, expectedOut, timeout) {
  def buildExpectedOutputMap(results: String): Map[String, String] = {
    // expected output is a string like "proc: SUCCESS, proc: FAIL"
    results.split(",").map(result =>
      (result.substring(0, result.indexOf(":")).trim,
       result.substring(result.indexOf(":") + 1).trim)).toMap
  }

  // Return (methodname, result)
  // Could be static, if we had the regex
  def resultFromOutputLine(outputLine: String): (String,String) = {
    // e.g. outputLine should look something like:
    //   Procedure set_next$node~node SUCCESS.
    var methodName = outputLine.split(" ")(1)
    methodName = methodName.substring(0, methodName.indexOf("$"))

    val actual: String =
      if (outputLine.contains("FAIL"))
        "FAIL"
      else
        "SUCCESS"

    (methodName, actual)
  }

  override def checkResults(expectedOutput: String, output: ExecutionOutput): Either[List[String], Iterable[Result]] = {
    val expectedOutputMap = buildExpectedOutputMap(expectedOutput)

    // `parse` is responsible for populating `results` with
    // lines which match `builder.regex`.
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

    // TODO: check that all the results methods contain the method name.
    // If not, then the test is 'under specified' relative to the actual file, and we should note that.

    val resultUnits = results.map(outputLine => {
      val (methodName, actual) = resultFromOutputLine(outputLine)

      expectedOutputMap.get(methodName) match {
        case Some(expected) => {
          Some(Result(methodName, expected, actual))
        }

        // If the method name from the actual output is not in the expectedOutputMap,
        // it means the expectedOutputMap was under-specified.
        // Easier to ignore, for now.
        case None => None
      }
    }).flatten

    return Right(resultUnits)
  }
}
