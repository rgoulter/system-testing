package edu.nus.systemtesting

import edu.nus.systemtesting.output.ConsoleOutputGenerator

abstract class TestCase(val commandName : String = "",
                        val fileName : String = "",
                        val arguments : String = "",
                        val outputDirectory : String = "",
                        val outputFileName : String = "",
                        val expectedOutput : String = "")
    extends Runnable with ConsoleOutputGenerator {
  // TODO: Return type of Either would make more sense here?
  def checkResults(expectedOutput : String, output : ExecutionOutput) : (Option[String], Boolean)

  override def formCommand() : String = {
    Seq(commandName, arguments, fileName).mkString(" ")
  }

  def run() = {
    val res@(execOutp, time) = this.execute

    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectory, execOutp.output)

    res
  }

  def generateTestResult(output : ExecutionOutput, time : Long) : TestCaseResult = {
    val (err, passed) = checkResults(expectedOutput, output)

    val result = if (passed) TestPassed else TestFailed

    new TestCaseResult(commandName, fileName, arguments, output, time, result, remarks = err.toList)
  }

  def generateOutput() = {
    val (outp, time) = run

    generateTestResult(outp, time)
  }
}