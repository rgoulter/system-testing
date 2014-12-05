package systemTestingDSL

import java.io.File
import systemTestingDSL._

/**
 * Takes in a list of default options, parent directory and executable and runs all
 * nested files
 */
class GenericTestSuite(parentDirectoryName: String,
  outputFileDirectory: String,
  defaultCommand: String,
  inputFileExtension: String,
  outputFileExtension: String,
  defaultOptions: String,
  generatedTestScriptName: String = "testScript.sh") {

  def getFiles() = {
    fileSystemUtilities.createDirectory(outputFileDirectory)
    val files = fileSystemUtilities.getRecursiveListOfFilesWithRegex(new File(parentDirectoryName), inputFileExtension)
    println("number of relevant files found: " + files.size)
    files.foreach(file => println(file.getName()))
    files
  }
  def run: Unit = {
    // Check if the output files exist before generating new ones. If they do, diff them.
    val files = getFiles()
    files.foreach(file =>
      try {
        val builder = new SleekTestCaseBuilder runCommand defaultCommand onFile file.getAbsolutePath withArguments defaultOptions storeOutputInDirectory outputFileDirectory withOutputFileName file.getName().concat(outputFileExtension)
        builder.build run
      } catch {
        case ex: Exception =>
      })
    // Existing versions are automatically diffed whereas new files are added after asking the user. Highlight new out files to the user and then add them in
  }

  /**
   * Generates a shell - script which can be executed to run tests manually. Don't forget to perform a chmod +x on the script otherwise it won't run
   */
  def generateTestScript() {
    val files = getFiles()
    var execute: String = ""
    var script: String = SCRIPT_PRELUDE.concat(NEW_LINE)
    files.foreach(file =>
      {
        execute = defaultCommand.concat(SPACE).concat(file.getAbsolutePath()).concat(SPACE).concat(defaultOptions).concat(SPACE).concat(REDIRECTION_OPERATOR).concat(SPACE).concat(outputFileDirectory).concat(file.getName.concat(outputFileExtension)).concat(NEW_LINE)
        script = script.concat(execute)
      })
    fileSystemUtilities.printToFile(new File(generatedTestScriptName))(p => p.print(script))
  }
  def generateTestStatistics() {

  }
}
