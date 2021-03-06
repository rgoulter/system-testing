package edu.nus.systemtesting.hipsleek

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.ProgramFlags.{ isFlag, flagsOfProgram }
import edu.nus.systemtesting.ExecutionOutput
import edu.nus.systemtesting.Result
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.TestCaseConfiguration
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseResult
import java.nio.file.Path
import java.nio.file.Paths
import edu.nus.systemtesting.ExpectsOutput

object HipTestCase {
  implicit def constructTestCase(ps: PreparedSystem, tc: Testable with ExpectsOutput, conf: TestCaseConfiguration): HipTestCase =
    new HipTestCase(ps.binDir,
                    tc.commandName,
                    ps.corpusDir,
                    tc.fileName,
                    tc.arguments,
                    tc.expectedOutput,
                    conf.timeout)
}

class HipTestCase(binDir: Path = Paths.get(""),
                  cmd: Path = Paths.get(""),
                  corpDir: Path = Paths.get(""),
                  fn: Path = Paths.get(""),
                  args: String = "",
                  val expectedOutput: String = "",
                  timeout: Int = 300,
                  regex: String = "Procedure.*FAIL.*|Procedure.*SUCCESS.*")
    extends TestCase(binDir, cmd, corpDir, fn, args, timeout) with ExpectsOutput {
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

  override def checkResults(output: ExecutionOutput): Either[List[String], Iterable[Result]] = {
    val expectedOutputMap = buildExpectedOutputMap(expectedOutput)

    // `parse` is responsible for populating `results` with
    // lines which match `builder.regex`.
    val results = filterLinesMatchingRegex(output.output, regex)

    if (results.isEmpty) {
      val testFlags = arguments.split(" ").filter(isFlag)
      val SleekFlags = flagsOfProgram(absCmdPath)
      val invalidFlags = testFlags.filterNot(SleekFlags.contains)

      if (!invalidFlags.isEmpty) {
        val flagsStr = invalidFlags.map(f => s"Invalid flag $f").mkString("\n")

        return Left(List("Binary failed to execute. Please investigate", flagsStr))
      } else {
        // Could try searching the output for errors?
        return Left(List("Binary failed to execute. Please investigate",
                         "Output was:") ++
                    output.stdoutLines ++
                    output.stderrLines)
      }
    }

    // TODO: check that all the results methods contain the method name.
    // If not, then the test is 'under specified' relative to the actual file, and we should note that.

    val resultUnits = results.flatMap(outputLine => {
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
    })

    return Right(resultUnits)
  }
}
