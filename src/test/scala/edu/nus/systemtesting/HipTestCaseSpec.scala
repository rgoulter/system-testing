package edu.nus.systemtesting

import org.scalatest.FlatSpec
import edu.nus.systemtesting.hipsleek.HipTestCase.constructHipTestCase
import com.typesafe.config.ConfigFactory
import edu.nus.systemtesting.hipsleek.HipTestCase

/**
 * @author richardg
 */
class HipTestCaseSpec extends FlatSpec with TestCaseBehaviors[HipTestCase] {
  // Assumes presence of a config
  val configuration = ConfigFactory.load()
  val HIP_COMMAND = configuration.getString("HIP_COMMAND")
  val WORKING_DIR = configuration.getString("HIP_DIR")

  def testCase() : TestCaseBuilder = {
    (new TestCaseBuilder
       runCommand HIP_COMMAND
       onFile WORKING_DIR + "infinity/inflist.ss"
       withArguments "--dsd --en-inf"
       storeOutputInDirectory "/tmp/"
       withOutputFileName "inflist.out")
  }

  implicit def constructTestCase(tcb : TestCaseBuilder) : HipTestCase = {
    HipTestCase.constructHipTestCase(tcb)
  }

  val outp = ExecutionOutput.outputFromString(OutputDumps.HipExResource)
  val passTestExpected = "remove: SUCCESS, append: SUCCESS"
  val failTestExpected = "remove: SUCCESS, append: FAIL"
  val failTestDiff = Array(("FAIL", "SUCCESS"))

  "Hip TestCase" should behave like validTest(outp, passTestExpected, failTestExpected, failTestDiff)

  it should "correctly get (methodname, result) from output line" in {
    val outputLine = "Procedure set_next$node~node SUCCESS."

    val test = constructTestCase(testCase)
    val (method, res) = test.resultFromOutputLine(outputLine)
    assertResult("set_next")(method)
    assertResult("SUCCESS")(res)
  }
}