package edu.nus.systemtesting

import java.nio.file.Path

import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.ReporterColors._
import GlobalReporter.reporter

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
                          val results: Either[Iterable[String], Iterable[Result]]) {
  val result: TestResult = results match {
    case Left(_) => TestFailed
    case Right(res) => if (res.forall(_.passed)) TestPassed else TestFailed
  }

  val diff: Array[Result] = results match {
    // Doesn't make sense to ask for diff in case of Left
    case Left(_) => Array[Result]()
    case Right(resultUnits) =>
      resultUnits.filterNot(_.passed).toArray
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
    (arguments trim() split "\\s*" sorted) mkString " "

  def expected() = results match {
    case Left(_) => List[String]()
    case Right(results) => results map(_.expected) toList
  }

  /**
   * Key which almost certain to be unique to a [[TestCaseResult]] in a
   * [[TestSuite]]
   */
  lazy val cmdFnArgsKey =
    (command, filename, sortedArgs)

  /**
   * Key which might be unique to a [[TestCaseResult]] in a [[TestSuite]].
   * Useful for finding tests which differ only in `arguments`.
   */
  lazy val cmdFnKey =
    (command, filename, sortedArgs)

  /**
   * @param threshold the min value `executionTime` needs to be to be shown.
   */
  def displayResult(threshold: Long = 1000L) = {
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
      case TestPassed => reporter.inColor(ColorGreen)("Passed")
      case TestFailed => reporter.inColor(ColorRed  )("Failed")
    }

    reporter.println(resStr)


    if (executionTime > threshold) {
      reporter.log(s"Runtime: $executionTime milliseconds")
    }


    def expect(m: String) = reporter.inColor(ColorCyan)(m)
    def actual(m: String) = reporter.inColor(ColorMagenta)(m)

    results match {
      case Left(remarks) => {
        remarks.foreach(reporter.log)

        reporter.println()
      }
      case Right(results) => {
        val diff = results.filterNot(_.passed)

        diff.foreach({ case Result(key, expected, got) =>
          reporter.println(s"Expected ${expect(expected)}, but got ${actual(got)} for $key")
        })

        if (!diff.isEmpty)
          reporter.println()
      }
    }
  }
}
