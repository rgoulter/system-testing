package edu.nus.systemtesting

import org.scalatest.FlatSpec
import edu.nus.systemtesting.hipsleek.SleekTestCase
import edu.nus.systemtesting.hipsleek.ConstructSleekTests
import com.typesafe.config.ConfigFactory

/**
 * @author richardg
 */
class SleekTestCaseSpec extends FlatSpec with TestCaseBehaviors[SleekTestCase] with ConstructSleekTests {
  def testCase(): TestCaseBuilder = {
    // Since `outp` below comes from `OutputDumps`,
    // the constants here are all arbitrary.
    (new TestCaseBuilder
       runCommand "sleek"
       onFile "sleek.slk"
       withArguments " ")
  }

  val outp = ExecutionOutput.outputFromString(OutputDumps.SleekExResource)
  val passTestExpected = "Valid, Valid, Valid, Fail"
  val failTestExpected = "Valid, Valid, Valid, Valid"
  // This confused me. But the actual result *SHOULD* be "Fail",
  // So we expect a diff where it says expected "Valid" but got "Fail"
  val failTestDiff = Array(Result("3", "Valid", "Fail"))

  "Sleek TestCase" should behave like validTest(outp, passTestExpected, failTestExpected, failTestDiff)
}
