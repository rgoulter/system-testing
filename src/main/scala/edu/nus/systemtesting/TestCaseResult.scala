package edu.nus.systemtesting

import java.nio.file.Path

import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.ReporterColors._
import edu.nus.systemtesting.output.VisibilityOptions._
import GlobalReporter.reporter
import GlobalReporter.visibility

/**
 * A system level test will either Pass or Fail.
 */
sealed trait TestResult
case object TestPassed extends TestResult
case object TestFailed extends TestResult

/**
 * @author richardg
 */
case class TestCaseResult(val command: Path,
                          val filename: Path,
                          val arguments: String,
                          val executionTime: Long,
                          val results: Either[Iterable[String], Iterable[Result]])
    extends Testable {
  // for Testable trait, the names are slightly different
  val commandName = command
  val fileName = filename

  val result: TestResult = results match {
    case Left(_) => TestFailed
    case Right(res) => if (res.forall(_.passed)) TestPassed else TestFailed
  }

  val diff: List[Result] = results match {
    // Doesn't make sense to ask for diff in case of Left
    case Left(_) => List[Result]()
    case Right(resultUnits) =>
      resultUnits.filterNot(_.passed).toList
  }

  val remarks: Array[String] = results match {
    case Left(rem) => rem.toArray
    // Doesn't make sense to ask for remarks in case of Right
    case Right(_) => Array[String]()
  }

  val executionSucceeded = results.isRight

  val passed = result == TestPassed

  /** Equivalence val, so that `" --a --b"` is the same as `"--b --a"`. */
  lazy val sortedArgs =
    (arguments trim() split "\\s+" sorted) mkString " "

  def expected() = results match {
    case Left(_) => List[(String, String)]()
    case Right(results) => results map { r => (r.key, r.expected) } toList
  }

  /**
   * Key which almost certain to be unique to a
   * [[TestCaseResult]] in a
   * [[edu.nus.systemtesting.testsuite.TestSuite]]
   */
  lazy val cmdFnArgsKey =
    (command, filename, sortedArgs)

  /**
   * Key which might be unique to a [[TestCaseResult]] in a
   * [[edu.nus.systemtesting.testsuite.TestSuite]].
   * Useful for finding tests which differ only in `arguments`.
   */
  lazy val cmdFnKey =
    (command, filename)

  /**
   * @param threshold the min value `executionTime` (in seconds) needs to be to be shown.
   */
  def displayResult(threshold: Long = 1L): Unit = {
    val canOutput = result match {
      case TestPassed => visibility.show(ShowPassingResults)
      case TestFailed if executionSucceeded => visibility.show(ShowFailingResults)
      case TestFailed if !executionSucceeded => visibility.show(ShowInvalidResults)
    }

    if (!canOutput)
      return

    // Assuming that execCmd is of same form as run in Runnable
    // Output (cmd, args?, filename) relative to projectDir, corpusDir
    val trimmedArgs = arguments.trim()
    val execCmd =
      if (trimmedArgs.isEmpty())
        Seq(command, filename).mkString(" ")
      else
        Seq(command, trimmedArgs, filename).mkString(" ")

    reporter.print(execCmd)

    // Ensure the "Passed"/"Failed" result is printed on ResultCol
    val ResultCol = 70
    val pad = ResultCol - execCmd.length()
    reporter.print(if (pad >= 0) {" " * pad} else {"\n" + " " * ResultCol})

    val resStr = result match {
      case TestPassed => reporter.inColor(ColorGreen)(" Passed")
      case TestFailed if executionSucceeded => reporter.inColor(ColorRed  )(" Failed")
      case TestFailed if !executionSucceeded => reporter.inColor(ColorRed  )("Invalid")
    }

    reporter.println(resStr)


    visibility.when(ShowExecutionTime) {
      if (executionTime > threshold * 1000) {
        reporter.log(s"Runtime: $executionTime milliseconds")
      }
    }


    def expect(m: String) = reporter.inColor(ColorCyan)(m)
    def actual(m: String) = reporter.inColor(ColorMagenta)(m)

    results match {
      case Left(remarks) => {
        val TruncateOutputTo = 16
        val rmIter = remarks.iterator
        val truncatedRemarks = rmIter.take(TruncateOutputTo).toList
        val remaining = rmIter.size

        visibility.when(ShowInvalidReasons) {
          truncatedRemarks foreach reporter.log
          if (remaining > 0)
            reporter.log(s"$remaining lines remaining (truncated)...")

          reporter.println()
        }
      }
      case Right(results) => {
        val diff = results.filterNot(_.passed)

        visibility.when(ShowFailingDiff) {
          diff foreach { case Result(key, expected, got) =>
            reporter.println(s"Expected ${expect(expected)}, but got ${actual(got)} for $key")
          }

          if (!diff.isEmpty)
            reporter.println()
        }
      }
    }
  }
}
