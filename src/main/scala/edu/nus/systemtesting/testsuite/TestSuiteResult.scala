package edu.nus.systemtesting.testsuite

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
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
                           val repoRevision: String,
                           private[testsuite] val resultFutures: List[Future[TestCaseResult]]) {
  lazy val results = resultFutures map { future =>
    // timeout taken care of by TestCase
    Await result (future, Duration.Inf)
  }
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
    // want to display result as-we-go;
    // waiting for result to be evaluated takes too long.
    resultFutures map { future =>
      // timeout taken care of by TestCase
      val tcr = Await result (future, Duration.Inf)
      tcr.displayResult(threshold)
    }

    generateTestStatistics()
  }
}

object TestSuiteResult {
  def withResults(hostname: String,
                  datetime: DateTime,
                  repoRevision: String,
                  resultsIter: List[TestCaseResult]): TestSuiteResult = {
    import ExecutionContext.Implicits.global

    // Since TestSuiteResult needs Futures in it's c'tor.
    val resultFutures = resultsIter.map(Future(_))

    new TestSuiteResult(hostname, datetime, repoRevision, resultFutures)
  }
}