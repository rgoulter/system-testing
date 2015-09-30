package edu.nus.systemtesting.testsuite

import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.hipsleek.TestSuiteResultAnalysis

/**
 * @author richardg
 */
abstract class TestSuiteUsage(val revision: String,
                              val significantTime: Long) {
  /** To be implemented by subclass. */
  def allTests: List[TestCase]

  def run(): TestSuiteResult = {
    val suite = new TestSuite(allTests, revision, significantTime)

    val suiteResult = suite.runAllTests

    TestSuiteResultAnalysis printTallyOfInvalidTests suiteResult

    suiteResult
  }
}