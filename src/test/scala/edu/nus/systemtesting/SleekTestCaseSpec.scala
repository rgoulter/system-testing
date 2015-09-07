package edu.nus.systemtesting

import org.scalatest.FlatSpec
import SleekTestCase.constructSleekTestCase
import com.typesafe.config.ConfigFactory

/**
 * @author richardg
 */
class SleekTestCaseSpec extends FlatSpec {
  // Assumes presence of a config
  val configuration = ConfigFactory.load()
  val SLEEK_COMMAND = configuration.getString("SLEEK_COMMAND")
  val WORKING_DIR = configuration.getString("SLEEK_DIR")

  // TestCase.generateTestResult needs an execution time
  val ArbitraryExecutionTime = 200L

  def mkTest(cmd : String,
             file : String,
             args : String,
             outputDir : String,
             outputFile : String,
             expectedOutput : String) : SleekTestCase =
    (new TestCaseBuilder
       runCommand cmd
       onFile file
       withArguments args
       storeOutputInDirectory outputDir
       withOutputFileName outputFile
       checkAgainst expectedOutput)

  "Sleek TestCase" should "pass for a simple, valid test case" in {
    // n.b. the arguments for output dir, etc. somewhat arbitrary, since not used.
    val testExpected = "Valid, Valid, Valid, Fail"
    val test = mkTest (SLEEK_COMMAND, WORKING_DIR + "sleek.slk", " ", "/tmp/", "sleek", testExpected)

    val outp = ExecutionOutput.outputFromString(OutputDumps.SleekExResource)

    val actualResult = test.generateTestResult(outp, ArbitraryExecutionTime)

    assertResult(TestPassed)(actualResult.result)
    assert(actualResult.diff.isEmpty)
    assert(actualResult.remarks.isEmpty)
  }

  it should "fail if actual result different from expected" in {
    // actual results should be V,V,V,F
    val testExpected = "Valid, Valid, Valid, Valid"
    val test = mkTest (SLEEK_COMMAND, WORKING_DIR + "sleek.slk", " ", "/tmp/", "sleek", testExpected)

    val outp = ExecutionOutput.outputFromString(OutputDumps.SleekExResource)

    val actualResult = test.generateTestResult(outp, ArbitraryExecutionTime)

    assertResult(TestFailed)(actualResult.result)
    assertResult(Array(("Valid", "Fail"))) {
      // This confused me. But the actual result *SHOULD* be "Fail",
      // So we expect a diff where it says expected "Valid" but got "Fail"
      actualResult.diff
    }
  }
}