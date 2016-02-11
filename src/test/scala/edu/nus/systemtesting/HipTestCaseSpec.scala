package edu.nus.systemtesting

import org.scalatest.FlatSpec
import edu.nus.systemtesting.hipsleek.HipTestCase
import java.nio.file.Paths

/**
 * @author richardg
 */
class HipTestCaseSpec extends FlatSpec with TestCaseBehaviors[HipTestCase] {
  def testCase(): TestCaseBuilder = {
    // Since `outp` below comes from `OutputDumps`,
    // the constants here are all arbitrary.
    (new TestCaseBuilder
       runCommand Paths.get("hip")
       onFile Paths.get("infinity/inflist.ss")
       withArguments "--dsd --en-inf")
  }

  implicit def constructTestCase(tcb: Testable with ExpectsOutput): HipTestCase = {
    new HipTestCase(
      Paths.get("."),
      tcb.commandName,
      Paths.get("."),
      tcb.fileName,
      tcb.arguments,
      tcb.expectedOutput,
      100
    )
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
