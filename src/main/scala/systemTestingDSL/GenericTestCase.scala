package systemTestingDSL

import systemTestingDSL.outputGenerator.ConsoleOutputGenerator

case class GenericTestCase(commandName: String, fileName: String, arguments: String, outputDirectoryName: String, outputFileName: String, outputFileExtension: String = ".out")
    extends Runnable with ConsoleOutputGenerator {
  var output: String = ""

  def run() = {
    this.output = this.execute
    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectoryName, output)
  }

  def runAndReturnOutput(): String = {
    this.output = this.execute
    output
  }
}