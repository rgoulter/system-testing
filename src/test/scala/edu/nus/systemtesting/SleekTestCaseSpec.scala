package edu.nus.systemtesting

import java.nio.file.Paths

import org.scalatest.FlatSpec

import edu.nus.systemtesting.hipsleek.SleekTestCase

/**
 * @author richardg
 */
class SleekTestCaseSpec extends FlatSpec with TestCaseBehaviors[SleekTestCase] {
  def testCase(): TestCaseBuilder = {
    // Since `outp` below comes from `OutputDumps`,
    // the constants here are all arbitrary.
    (new TestCaseBuilder
       runCommand Paths.get("sleek")
       onFile Paths.get("sleek.slk")
       withArguments " ")
  }

  implicit def constructTestCase(tcb: Testable with ExpectsOutput): SleekTestCase = {
    new SleekTestCase(
      Paths.get("."),
      tcb.commandName,
      Paths.get("."),
      tcb.fileName,
      tcb.arguments,
      tcb.expectedOutput,
      100
    )
  }

  val outp = ExecutionOutput.outputFromString(OutputDumps.SleekExResource)
  val passTestExpected = "Valid, Valid, Valid, Fail"
  val failTestExpected = "Valid, Valid, Valid, Valid"
  // This confused me. But the actual result *SHOULD* be "Fail",
  // So we expect a diff where it says expected "Valid" but got "Fail"
  val failTestDiff = Array(Result("3", "Valid", "Fail"))

  "Sleek TestCase" should behave like validTest(outp, passTestExpected, failTestExpected, failTestDiff)
}
