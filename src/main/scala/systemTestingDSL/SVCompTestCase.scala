package systemTestingDSL

import systemTestingDSL.outputGenerator.ConsoleOutputGenerator

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
    commandName.concat(separator).concat(fileName).concat(separator).concat(arguments)
  }
  /**
   * *
   * This function does not write to file but just returns the console output
   */
  def runAndReturn(): String = {
    this.execute._1
  }

  def run(): Unit = {
    this.output = this.execute
    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectoryName, output._1, outputFileExtension)
  }

}