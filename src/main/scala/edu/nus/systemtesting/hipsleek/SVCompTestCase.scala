package edu.nus.systemtesting.hipsleek

import edu.nus.systemtesting.output.ConsoleOutputGenerator
import edu.nus.systemtesting.Runnable

case class SVCompTestCase(commandName: String,
                          fileName: String,
                          arguments: String,
                          outputDirectoryName: String = "",
                          outputFileName: String = "",
                          outputFileExtension: String = ".out")
    extends ConsoleOutputGenerator {
  /**
   * Overriding traits implementation because the order of the arguments and file name is different
   */
  def formCommand(): String = {
    Seq(commandName, fileName, arguments).mkString(" ")
  }

  /**
   * This function does not write to file but just returns the console output
   */
  def runAndReturn(): String = {
    val (outp, time) = Runnable execute formCommand
    outp.output
  }

  def run(): Unit = {
    val (outp, time) = Runnable execute formCommand

    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectoryName, outp.output, outputFileExtension)
  }
}
