package edu.nus.systemtesting.testsuite

import java.io.PrintWriter
import scala.collection.mutable.MutableList
import scala.sys.process.stringToProcess
import org.joda.time.DateTime

import edu.nus.systemtesting.{ Result, TestCase,
                               TestCaseResult, TestFailed, TestPassed }
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.ReporterColors._
import GlobalReporter.reporter

class TestSuite(tests: List[TestCase],
                revision: String,
                significantTime: Long) {
  // significantTime in seconds
  val THRESHOLD = (significantTime * 1000)

  def runAllTests(): TestSuiteResult = {
    val startTime = System.currentTimeMillis

    val testResults = tests.map(test => {
      val testResult = test.generateOutput

      displayResult(testResult)

      testResult
    })

    val endTime = System.currentTimeMillis

    val timeTaken = (endTime - startTime) / 1000

    reporter.println()
    reporter.log(s"Total time taken to run all tests: $timeTaken seconds")
    reporter.println()

    // assuming the `hostname` command can't/won't fail
    val hostname : String = "hostname" !!
    val now = DateTime.now()
    val suiteResult = TestSuiteResult(hostname, now, revision, testResults)

    suiteResult
  }

  def displayResult(result: TestCaseResult) = {
    // Assuming that execCmd is of same form as run in Runnable
    // Output (cmd, args?, filename) relative to projectDir, corpusDir
    val trimmedArgs = result.arguments.trim()
    val execCmd =
      if (trimmedArgs.isEmpty())
        Seq(result.command, result.filename).mkString(" ")
      else
        Seq(result.command, trimmedArgs, result.filename).mkString(" ")

    reporter.print(execCmd)

    // Ensure the "Passed"/"Failed" result is printed on ResultCol
    val ResultCol = 70
    val pad = ResultCol - execCmd.length()
    reporter.print(if (pad >= 0) {" " * pad} else {"\n" + " " * ResultCol})

    val resStr = result.result match {
      case TestPassed => reporter.inColor(ColorGreen)("Passed")
      case TestFailed => reporter.inColor(ColorRed  )("Failed")
    }

    reporter.println(resStr)


    val time = result.executionTime

    if (time > THRESHOLD) {
      reporter.log("Runtime: " + time + " milliseconds")
    }


    def expect(m: String) = reporter.inColor(ColorCyan)(m)
    def actual(m: String) = reporter.inColor(ColorMagenta)(m)

    result.results match {
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
