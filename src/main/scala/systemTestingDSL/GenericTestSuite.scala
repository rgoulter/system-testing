package systemTestingDSL

import java.io.File

/**
 * Takes in a list of default options, parent directory and executable and runs all
 * nested files
 */
class GenericTestSuite(parentDirectoryName: String, outputFileDirectory: String, defaultCommand: String, defaultOptions: String) {

  def run: Unit = {
    val files = fileSystemUtilities.getRecursiveListOfFilesWithRegex(new File(parentDirectoryName), ".*\\.slk")
    println("number of sleek files found: " + files.size)
    files.foreach(file => println(file.getName()))
    files.foreach(file =>
      try {
        new SleekTestCase(commandName = defaultCommand,
          fileName = file.getAbsolutePath(),
          arguments = defaultOptions,
          outputFileName = file.getName().concat(".out")).run()
      } catch {
        case ex: Exception =>
      })
  }
}
