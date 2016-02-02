package edu.nus.systemtesting.hipsleek

import edu.nus.systemtesting.ExpectsOutput
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseConfiguration
import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.output.VisibilityOptions
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.ConstructTestCase

/**
 * @author richardg
 */
package object app {
  /** Used for `diffSuiteResults`, to save typing / screen space. */
  type DiffableResults = (String, TestSuiteResult, TestSuiteResult)

  private[app] def recoverFromTCR(tcr: TestCaseResult):
      (Testable with ExpectsOutput, ConstructTestCase) = {
    // n.b. cannot recover `expectedOutput` from tcr directly.

    val tcrExp = tcr.expected

    val testKind =
      if (tcr.command.endsWith("hip")) {
        HipConfigArg()
      } else if (tcr.command.endsWith("sleek")) {
        // MAGIC
        if (!tcrExp.exists { case (_, v) => v == "OK" })
          // run-fast-tests style Sleek Test Case
          SleekConfigArg(isValidate = false)
        else
          SleekConfigArg(isValidate = false)
      } else
        throw new UnsupportedOperationException(s"Expected command ${tcr.command} to be `sleek` or `hip`.")

    def recoverHipExpectedOuput(exp: List[(String, String)]): String = {
      exp map { case (k, v) => k + ": " + v } mkString(", ")
    }

    def recoverSleekExpectedOuput(exp: List[(String, String)]): String = {
      exp map { case (k, v) => v } mkString(", ")
    }

    // recover*ExpectedOutput above is awkward/magic, may be *TestCase could overload exp.
    // to allow for more appropriate types...?
    val expectedOutp = testKind match {
      case HipConfigArg()        => recoverHipExpectedOuput(tcrExp)
      case SleekConfigArg(false) => recoverSleekExpectedOuput(tcrExp)
      // Validate Sleek test case doesn't need any expectedOutp
      case SleekConfigArg(true)  => ""
    }

    println(s"Recovered Expected output:" + expectedOutp)

    val testable = new TestCaseBuilder(tcr.command, tcr.filename, tcr.arguments, expectedOutp)

    // MAGIC, & awkward, but difficult to think of a better way of doing this currently
    val construct: (PreparedSystem, Testable with ExpectsOutput, TestCaseConfiguration) => TestCase =
      testKind match {
        case HipConfigArg()        => HipTestCase.constructTestCase
        case SleekConfigArg(false) => SleekTestCase.constructTestCase
        case SleekConfigArg(true)  => ValidateableSleekTestCase.constructTestCase
      }

    (testable, construct)
  }
}