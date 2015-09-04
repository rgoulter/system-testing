package edu.nus.systemtesting

import edu.nus.systemtesting.output.ConsoleOutputGenerator

case class SVCompTestCase(commandName: String,
  fileName: String,
  arguments: String,
  outputDirectoryName: String = "",
  outputFileName: String = "",
  outputFileExtension: String = ".out")
    extends Runnable with ConsoleOutputGenerator {
  var output: (String, Long) = ("", 0)

  /**
   * Overriding traits implementation because the order of the arguments and file name is different
   */
  override def formCommand(): String = {
    Seq(commandName, fileName, arguments).mkString(" ")
  }

  /**
   * This function does not write to file but just returns the console output
   */
  def runAndReturn(): String = {
    val (outp, time) = this.execute
    outp
  }

  def run(): Unit = {
    this.output = this.execute

    val (outp, time) = this.output

    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectoryName, outp, outputFileExtension)
  }
}