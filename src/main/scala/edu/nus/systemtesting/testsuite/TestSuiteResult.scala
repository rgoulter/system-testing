package edu.nus.systemtesting.testsuite

import org.joda.time.DateTime
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.TestPassed
import edu.nus.systemtesting.output.GlobalReporter
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
    reporter.log("Total number of tests: " + (successes.length + failures.length))
    reporter.println(reporter.inColor(reporter.ColorGreen)("Total number of tests passed: " + successes.length))
    reporter.println(reporter.inColor(reporter.ColorRed)("Total number of tests failed: " + failures.length))
  }
}