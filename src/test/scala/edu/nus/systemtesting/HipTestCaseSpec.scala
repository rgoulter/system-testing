package edu.nus.systemtesting

import org.scalatest.FlatSpec
import edu.nus.systemtesting.hipsleek.HipTestCase.constructHipTestCase
import com.typesafe.config.ConfigFactory
import edu.nus.systemtesting.hipsleek.HipTestCase

/**
 * @author richardg
 */
class HipTestCaseSpec extends FlatSpec {
  // Assumes presence of a config
  val configuration = ConfigFactory.load()
  val HIP_COMMAND = configuration.getString("HIP_COMMAND")
  val WORKING_DIR = configuration.getString("HIP_DIR")

  // TestCase.generateTestResult needs an execution time
  val ArbitraryExecutionTime = 200L

  def mkTest(cmd : String,
             file : String,
             args : String,
             outputDir : String,
             outputFile : String,
             expectedOutput : String) : HipTestCase =
    (new TestCaseBuilder
       runCommand cmd
       onFile file
       withArguments args
       storeOutputInDirectory outputDir
       withOutputFileName outputFile
       checkAgainst expectedOutput)

  "Sleek TestCase" should "pass for a simple, valid test case" in {
    // n.b. the arguments for output dir, etc. somewhat arbitrary, since not used.
    val testExpected = "remove: SUCCESS, append: SUCCESS"
    val test = mkTest (HIP_COMMAND, WORKING_DIR + "infinity/inflist.ss", "--dsd --en-inf", "/tmp/", "inflist.out", testExpected)

    val outp = ExecutionOutput.outputFromString(OutputDumps.HipExResource)

    val actualResult = test.generateTestResult(outp, ArbitraryExecutionTime)

    assertResult(TestPassed)(actualResult.result)
    assert(actualResult.diff.isEmpty)
    assert(actualResult.remarks.isEmpty)
  }

  it should "correctly get (methodname, result) from output line" in {
    val outputLine = "Procedure set_next$node~node SUCCESS."

    val test = mkTest (HIP_COMMAND, WORKING_DIR + "infinity/inflist.ss", "--dsd --en-inf", "/tmp/", "inflist.out", "")

    val (method, res) = test.resultFromOutputLine(outputLine)
    assertResult("set_next")(method)
    assertResult("SUCCESS")(res)
  }

  it should "fail if actual result different from expected" in {
    // actual results should be remove:S,append:S
    val testExpected = "remove: SUCCESS, append: FAIL"
    val test = mkTest (HIP_COMMAND, WORKING_DIR + "infinity/inflist.ss", "--dsd --en-inf", "/tmp/", "inflist.out", testExpected)

    val outp = ExecutionOutput.outputFromString(OutputDumps.HipExResource)

    val actualResult = test.generateTestResult(outp, ArbitraryExecutionTime)

    assertResult(TestFailed)(actualResult.result)
    assertResult(Array(("FAIL", "SUCCESS"))) {
      // This confused me. But the actual result *SHOULD* be "Fail",
      // So we expect a diff where it says expected "Valid" but got "Fail"
      actualResult.diff
    }
  }
}