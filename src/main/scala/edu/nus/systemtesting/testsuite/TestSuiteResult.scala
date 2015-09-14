package edu.nus.systemtesting.testsuite

import org.joda.time.DateTime
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.TestPassed
import edu.nus.systemtesting.output.ConsoleOutputGenerator
import java.io.PrintWriter


/**
 * @author richardg
 */
case class TestSuiteResult(val hostname: String,
                           val datetime: DateTime,
                           val repoRevision : String,
                           val results : Iterable[TestCaseResult])
    extends ConsoleOutputGenerator {
  lazy val (successes, failures) = results.toList.partition(_.result equals TestPassed)

  def generateTestStatistics(writer: PrintWriter): Unit = {
    writer.println(log("Total number of tests: " + (successes.length + failures.length)))
    writer.println(success("Total number of tests passed: " + successes.length))
    writer.println(error("Total number of tests failed: " + failures.length))
  }
}