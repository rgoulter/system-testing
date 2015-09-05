package edu.nus.systemtesting

import java.io.File

import scala.collection.mutable.MutableList

/**
 * Takes in a list of default options, parent directory and executable and runs all
 * nested files
 *
 * Does literally the same thing as RegressionTestReferenceBuilder,
 * but by way of generating a batch script to do it. Although, doesn't
 * use Config to generate these files.
 */
class ScriptGeneratorTestSuite(parentDirectoryName: String,
    outputFileDirectory: String,
    defaultCommand: String,
    inputFileExtension: String,
    outputFileExtension: String,
    defaultOptions: String,
    generatedTestScriptName: String = "testScript.sh") {
  var totalTestsToRun = 0;
  var testFailures = new MutableList[String];

  def getFiles() = {
    FileSystemUtilities createDirectory outputFileDirectory

    lazy val files = FileSystemUtilities.getRecursiveListOfFilesWithRegex(new File(parentDirectoryName), inputFileExtension)

    totalTestsToRun = files.size;
    println("number of relevant files found: " + totalTestsToRun)

    files
  }

  def run: Unit = {
    // Check if the output files exist before generating new ones. If they do, diff them.
    val files = getFiles()

    files.foreach(file =>
      try {
        val builder =
          (new SleekTestCaseBuilder runCommand defaultCommand
            onFile file.getAbsolutePath
            withArguments defaultOptions
            storeOutputInDirectory outputFileDirectory
            withOutputFileName (file.getName() + outputFileExtension))

        builder.build run
      } catch {
        case ex: Exception =>
      })

    // Existing versions are automatically diffed whereas new files are added
    // after asking the user.
    // Highlight new out files to the user and then add them in
  }

  /**
   * Generates a shell - script which can be executed to run tests manually.
   * Don't forget to perform a chmod +x on the script otherwise it won't run.
   */
  def generateTestScript() {
    val files = getFiles()

    var execute: String = ""
    var script: String = SCRIPT_PRELUDE + NEW_LINE

    files.foreach(file => {
      execute = Seq(defaultCommand,
                    file.getAbsolutePath(),
                    defaultOptions,
                    REDIRECTION_OPERATOR,
                    (outputFileDirectory + file.getName + outputFileExtension)).mkString(SPACE) +
                NEW_LINE
      script += execute
    })
    FileSystemUtilities.printToFile(new File(generatedTestScriptName))(p => p.print(script))
  }

  def generateTestStatistics() {
  }
}
