package edu.nus.systemtesting

import edu.nus.systemtesting.output.ConsoleOutputGenerator

case class GenericTestCase(commandName : String,
                           fileName : String,
                           arguments : String,
                           outputDirectoryName : String = "",
                           outputFileName : String = "",
                           outputFileExtension : String = ".out")
    extends Runnable with ConsoleOutputGenerator {
  override def formCommand() : String = {
    Seq(commandName, arguments, fileName).mkString(" ")
  }

  /** This function does not write to file but just returns the console output */
  def runAndReturn() : String = {
    val (execOutput, time) = this.execute
    execOutput.output
  }

  /**
   * Execute the program. Write output to `outputFilename`, if have a non-empty `outputFilename`.
   */
  def run() : (ExecutionOutput, Long) = {
    val res@(execOutput, time) = this.execute

    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectoryName, execOutput.output, outputFileExtension)

    res
  }
}