package edu.nus.systemtesting.testsuite

import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.hipsleek.TestSuiteResultAnalysis
import edu.nus.systemtesting.ExpectsOutput

/**
 * @author richardg
 */
abstract class TestSuiteUsage(val revision: String,
                              val significantTime: Long) {
  /** To be implemented by subclass. */
  def allTests: List[TestCase with ExpectsOutput]

  def suite: TestSuite =
    new TestSuite(allTests, revision, significantTime)
}