package edu.nus.systemtesting

import org.scalatest.FlatSpec

/**
 * Describes common behaviour TestCases should obey.
 * @author richardg
 */
trait TestCaseBehaviors[T <: TestCase] { this: FlatSpec =>
  // TestCase.generateTestResult needs an execution time
  val ArbitraryExecutionTime = 200L

  def testCase() : TestCaseBuilder

  implicit def constructTestCase(tcb : TestCaseBuilder) : T

  /**
   * A "valid" test will pass when expected, actual output are the same,
   * and fail when expeced, actual output are different.
   */
  def validTest(output : ExecutionOutput,
                passingExpect : String,
                failingExpect : String,
                failingDiff : Array[(String, String)]) {
    it should "pass for a simple, valid test case" in {
      val test = testCase checkAgainst passingExpect

      val actualResult = test.generateTestResult(output, ArbitraryExecutionTime)

      assertResult(TestPassed)(actualResult.result)
      assert(actualResult.diff.isEmpty)
      assert(actualResult.remarks.isEmpty)
    }

    it should "fail if actual result different from expected" in {
      val test = testCase checkAgainst failingExpect

      val actualResult = test.generateTestResult(output, ArbitraryExecutionTime)

      assertResult(TestFailed)(actualResult.result)
      assertResult(failingDiff)(actualResult.diff)
    }
  }
}