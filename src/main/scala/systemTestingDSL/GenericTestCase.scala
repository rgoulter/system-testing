package systemTestingDSL

import systemTestingDSL.outputGenerator.ConsoleOutputGenerator

case class GenericTestCase(commandName: String,
                           fileName: String,
                           arguments: String,
                           outputDirectoryName: String = "",
                           outputFileName: String = "",
                           outputFileExtension: String = ".out")
    extends Runnable with ConsoleOutputGenerator {
  var output: (String, Long) = ("", 0)

  override def formCommand(): String = {
    commandName.concat(separator).concat(arguments).concat(separator).concat(fileName)
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