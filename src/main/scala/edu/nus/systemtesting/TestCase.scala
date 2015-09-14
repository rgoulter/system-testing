package edu.nus.systemtesting

import edu.nus.systemtesting.output.ConsoleOutputGenerator
import Runnable.execute

/**
 * For representing each `(expected, actual)` pair within a test case.
 */
case class Result(val key: String, val expected: String, val actual: String) {
  val passed = expected equals actual
}

abstract class TestCase(val commandName: String = "",
                        val fileName: String = "",
                        val arguments: String = "",
                        val expectedOutput: String = "") {
  /**
   * Check whether the test passed using `expectedOutput`, against the [[ExecutionOutput]].
   *
   * Return Either a list of remarks about a failure to run the test case,
   * or a list of differences between the expected and actual output.
   * i.e. A passing test will return `Right` alternative with empty list.
   */
  def checkResults(expectedOutput: String, output: ExecutionOutput): Either[List[String], Iterable[Result]]

  def formCommand(): String = {
    Seq(commandName, arguments, fileName).mkString(" ")
  }

  def run() = {
    val res@(execOutp, time) = execute(formCommand())

    res
  }

  def generateTestResult(output: ExecutionOutput, time: Long): TestCaseResult = {
    val check = checkResults(expectedOutput, output)

    new TestCaseResult(commandName, fileName, arguments, time, check)
  }

  def generateOutput() = {
    val (outp, time) = run

    generateTestResult(outp, time)
  }
}
