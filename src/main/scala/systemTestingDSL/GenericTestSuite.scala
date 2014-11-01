package systemTestingDSL

import java.io.File

/**
 * Takes in a list of default options, parent directory and executable and runs all
 * nested files
 */
class GenericTestSuite(parentDirectoryName: String,
  outputFileDirectory: String,
  defaultCommand: String,
  inputFileExtension: String,
  outputFileExtension: String,
  defaultOptions: String) {

  def run: Unit = {
    fileSystemUtilities.createDirectory(outputFileDirectory)
    val files = fileSystemUtilities.getRecursiveListOfFilesWithRegex(new File(parentDirectoryName), inputFileExtension)
    println("number of relevant files found: " + files.size)
    files.foreach(file => println(file.getName()))
    files.foreach(file =>
      try {
        val builder = new SleekTestCaseBuilder runCommand defaultCommand onFile file.getAbsolutePath withArguments defaultOptions storeOutputIn file.getName().concat(outputFileExtension)
        builder.build run
      } catch {
        case ex: Exception =>
      })
    // Existing versions are automatically diffed whereas new files are added after asking the user. Highlight new out files to the user and then add them in
  }
}
