package edu.nus.systemtesting

import edu.nus.systemtesting.output.ConsoleOutputGenerator

/**
 * Like a `TestCase`, models the info for running some command on a file with
 * the arguments, and saving this output to some output file.
 * Unlike a `TestCase`, there's no `regex` and so no checks for whether a
 * test "passes" or not.
 *
 * Used by [[RegressionTestReferenceBuilder]].
 * These classes are useful only for batch-running commands on all files.
 * Can do without.
 */
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
