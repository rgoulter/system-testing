package edu.nus.systemtesting

import edu.nus.systemtesting.output.ConsoleOutputGenerator
import Runnable.execute

abstract class TestCase(val commandName : String = "",
                        val fileName : String = "",
                        val arguments : String = "",
                        val outputDirectory : String = "",
                        val outputFileName : String = "",
                        val expectedOutput : String = "") {
  /**
   * Check whether the test passed using `expectedOutput`, against the [[ExecutionOutput]].
   *
   * Return Either a list of remarks about a failure to run the test case,
   * or a list of differences between the expected and actual output.
   * i.e. A passing test will return `Right` alternative with empty list.
   */
  def checkResults(expectedOutput : String, output : ExecutionOutput) : Either[List[String], Iterable[(String, String)]]

  def formCommand() : String = {
    Seq(commandName, arguments, fileName).mkString(" ")
  }

  def run() = {
    val res@(execOutp, time) = execute(formCommand())

    res
  }

  def generateTestResult(output : ExecutionOutput, time : Long) : TestCaseResult = {
    val check = checkResults(expectedOutput, output)

    val (result, diff, remarks) = check match {
      case Left(remarks) => {
        (TestFailed, List(), remarks)
      }
      case Right(diff) => {
        val result = if (diff.isEmpty) TestPassed else TestFailed
        (result, diff, List())
      }
    }

    new TestCaseResult(commandName, fileName, arguments, output, time, result, diff, remarks)
  }

  def generateOutput() = {
    val (outp, time) = run

    generateTestResult(outp, time)
  }
}