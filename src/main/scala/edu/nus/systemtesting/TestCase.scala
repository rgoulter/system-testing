package edu.nus.systemtesting

import Runnable.execute
import java.nio.file.Path

/**
 * For representing each `(expected, actual)` pair within a test case.
 */
case class Result(val key: String, val expected: String, val actual: String) {
  val passed = expected equals actual
}

/**
 * @param commandName relative to `binDir`
 * @param corpusDir absolute path
 * @param fileName relative to corpusDir directory
 */
abstract class TestCase(val binDir: Path,
                        val commandName: Path,
                        val corpusDir: Path,
                        val fileName: Path,
                        val arguments: String = "",
                        val timeout: Int) extends Testable {
  /**
   * Check whether the test passed using `expectedOutput`, against the [[ExecutionOutput]].
   *
   * Return Either a list of remarks about a failure to run the test case,
   * or a list of differences between the expected and actual output.
   * i.e. A passing test will return `Right` alternative with empty list.
   */
  def checkResults(output: ExecutionOutput): Either[List[String], Iterable[Result]]

  val absCmdPath = binDir resolve commandName

  val absFilePath = corpusDir resolve fileName

  def formCommand(): String = {
    Seq(absCmdPath,
        arguments,
        absFilePath).mkString(" ")
  }

  def run() = {
    val res@(execOutp, time) = execute(formCommand(), timeout)

    res
  }

  def generateTestResult(output: ExecutionOutput,
                         time: Long,
                         checkCmdFileExists: Boolean = true): TestCaseResult = {
    // Although cmd, fn is already run, can check..
    // n.b. hip/sleek return 0 even if the file given isn't present.
    val cmdExists = absCmdPath.toFile().exists()
    val fileExists = absFilePath.toFile().exists()
    val check = if (!checkCmdFileExists || (cmdExists && fileExists)) {
      // Only check results if both cmd, file exist.
      checkResults(output)
    } else {
      Left(List(if (!cmdExists) Some(s"$commandName doesn't exist!") else None,
                if (!fileExists) Some(s"$fileName doesn't exist!") else None).flatten)
    }

    new TestCaseResult(commandName, fileName, arguments, time, check)
  }

  def generateOutput() = {
    val (outp, time) = run

    generateTestResult(outp, time)
  }
}
