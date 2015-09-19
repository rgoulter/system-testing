package edu.nus.systemtesting.testsuite

import java.io.PrintWriter
import scala.collection.mutable.MutableList
import scala.sys.process.stringToProcess
import org.joda.time.DateTime

import edu.nus.systemtesting.{ Result, TestCase,
                               TestCaseResult, TestFailed, TestPassed }
import edu.nus.systemtesting.output.GlobalReporter
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

      testResult.displayResult(THRESHOLD)

      testResult
    })

    val endTime = System.currentTimeMillis

    val timeTaken = (endTime - startTime) / 1000

    reporter.println()
    reporter.log(s"Total time taken to run all tests: $timeTaken seconds")
    reporter.println()

    // assuming the `hostname` command can't/won't fail
    val hostname : String = ("hostname" !!).trim()
    val now = DateTime.now()
    val suiteResult = TestSuiteResult(hostname, now, revision, testResults)

    suiteResult
  }
}
