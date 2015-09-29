package edu.nus.systemtesting.testsuite

import org.joda.time.DateTime
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.TestPassed
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.ReporterColors._
import edu.nus.systemtesting.output.VisibilityOptions._
import java.io.PrintWriter

import GlobalReporter.reporter
import GlobalReporter.visibility


/**
 * @author richardg
 */
case class TestSuiteResult(val hostname: String,
                           val datetime: DateTime,
                           val repoRevision : String,
                           resultsIter : Iterable[TestCaseResult]) {
  val results = resultsIter.toList
  lazy val (valid, invalid) = results.partition(_.executionSucceeded)
  lazy val (successes, failures) = valid.partition(_.result equals TestPassed)

  def generateTestStatistics(): Unit = visibility.when(ShowSummary) {
    // There's probably a tidier way to do this with some printf function, but..
    val TotalMsg = "Total number of tests:         "
    val PassMsg  = "Total number of tests passed:  "
    val FailMsg  = "Total number of tests failed:  "
    val InvlMsg  = "Total number of tests invalid: "
    reporter.log(TotalMsg + results.length)
    reporter.println(reporter.inColor(ColorGreen)(PassMsg + successes.length))
    reporter.println(reporter.inColor(ColorRed)(FailMsg + failures.length))
    reporter.println(reporter.inColor(ColorYellow)(InvlMsg + invalid.length))
  }

  /**
   * @param threshold for [[TestCaseResult.displayResult]], in seconds.
   */
  def displayResult(threshold: Long = 1L): Unit = {
    results.foreach(_.displayResult(threshold))

    generateTestStatistics()
  }
}