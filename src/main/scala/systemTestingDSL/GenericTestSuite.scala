package systemTestingDSL

import java.io.File

/**
 * Takes in a list of default options, parent directory and executable and runs all
 * nested files
 */
class GenericTestSuite(parentDirectoryName: String, outputFileDirectory: String, defaultCommand: String, defaultOptions: String) {

  def run: Unit = {
    val files = getRecursiveListOfFilesWithRegex(new File(parentDirectoryName), "*.slk")
    files.foreach(file =>
      new TestCase(commandName = defaultCommand,
        fileName = file.getName(),
        arguments = defaultOptions,
        outputFileName = file.getName().concat(".out")).run())
  }

}