package edu.nus.systemtesting

import org.scalatest.FlatSpec
import edu.nus.systemtesting.hipsleek.SleekTestCase
import edu.nus.systemtesting.hipsleek.ConstructSleekTests
import com.typesafe.config.ConfigFactory

/**
 * @author richardg
 */
class SleekTestCaseSpec extends FlatSpec with TestCaseBehaviors[SleekTestCase] with ConstructSleekTests {
  // Assumes presence of a config
  val configuration = ConfigFactory.load()
  val SLEEK_COMMAND = configuration.getString("SLEEK_COMMAND")
  val WORKING_DIR = configuration.getString("SLEEK_DIR")

  def testCase() : TestCaseBuilder = {
    (new TestCaseBuilder
       runCommand SLEEK_COMMAND
       onFile WORKING_DIR + "sleek.slk"
       withArguments " "
       storeOutputInDirectory "/tmp/"
       withOutputFileName "sleek.out")
  }

  val outp = ExecutionOutput.outputFromString(OutputDumps.SleekExResource)
  val passTestExpected = "Valid, Valid, Valid, Fail"
  val failTestExpected = "Valid, Valid, Valid, Valid"
  // This confused me. But the actual result *SHOULD* be "Fail",
  // So we expect a diff where it says expected "Valid" but got "Fail"
  val failTestDiff = Array(("Valid", "Fail"))

  "Sleek TestCase" should behave like validTest(outp, passTestExpected, failTestExpected, failTestDiff)
}