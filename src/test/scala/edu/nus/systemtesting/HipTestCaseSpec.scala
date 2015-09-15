package edu.nus.systemtesting

import org.scalatest.FlatSpec
import edu.nus.systemtesting.hipsleek.HipTestCase
import edu.nus.systemtesting.hipsleek.ConstructHipTests

/**
 * @author richardg
 */
class HipTestCaseSpec extends FlatSpec with TestCaseBehaviors[HipTestCase] with ConstructHipTests {
  def testCase(): TestCaseBuilder = {
    // Since `outp` below comes from `OutputDumps`,
    // the constants here are all arbitrary.
    (new TestCaseBuilder
       runCommand "hip"
       onFile "infinity/inflist.ss"
       withArguments "--dsd --en-inf")
  }

  val outp = ExecutionOutput.outputFromString(OutputDumps.HipExResource)
  val passTestExpected = "remove: SUCCESS, append: SUCCESS"
  val failTestExpected = "remove: SUCCESS, append: FAIL"
  val failTestDiff = Array(Result("append", "FAIL", "SUCCESS"))

  "Hip TestCase" should behave like validTest(outp, passTestExpected, failTestExpected, failTestDiff)

  it should "correctly get (methodname, result) from output line" in {
    val outputLine = "Procedure set_next$node~node SUCCESS."

    val (method, res) = testCase.resultFromOutputLine(outputLine)
    assertResult("set_next")(method)
    assertResult("SUCCESS")(res)
  }
}
