package edu.nus.systemtesting.testsuite

import org.joda.time.DateTime
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.TestPassed
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.ReporterColors._
import java.io.PrintWriter

import GlobalReporter.reporter


/**
 * @author richardg
 */
case class TestSuiteResult(val hostname: String,
                           val datetime: DateTime,
                           val repoRevision : String,
                           val results : Iterable[TestCaseResult]) {
  lazy val (successes, failures) = results.toList.partition(_.result equals TestPassed)

  def generateTestStatistics(writer: PrintWriter): Unit = {
    // There's probably a tidier way to do this with some printf function, but..
    val TotalMsg = "Total number of tests:        "
    val PassMsg  = "Total number of tests passed: "
    val FailMsg  = "Total number of tests failed: "
    reporter.log(TotalMsg + (successes.length + failures.length))
    reporter.println(reporter.inColor(ColorGreen)(PassMsg + successes.length))
    reporter.println(reporter.inColor(ColorRed)(FailMsg + failures.length))
  }
}