package edu.nus.systemtesting

/**
 * @author richardg
 */
package object testsuite {
  /**
   * Convenience type to make [[TestSuiteComparison]] easier to read.
   */
  type TestCaseDiffPair = (TestCaseResult, TestCaseResult)
}