package edu.nus.systemtesting

/**
 * Class to facilitate DSL-esque construction of test cases.
 */
case class TestCaseBuilder(val commandName : String = "",
                           val fileName : String = "",
                           val arguments : String = "",
                           val outputDirectory : String = "",
                           val outputFileName : String = "",
                           val expectedOutput : String = "") {
  //
  // Helper functions for DSL-esque construction of testcase.
  //

  def runCommand(commandName : String) =
    copy(commandName = commandName)

  def onFile(fileName : String) =
    copy(fileName = fileName)

  def withArguments(arguments : String) =
    copy(arguments = arguments)

  def storeOutputInDirectory(outputDirectory : String) =
    copy(outputDirectory = outputDirectory)

  def withOutputFileName(outputFileName : String) =
    copy(outputFileName = outputFileName)

  def checkAgainst(expectedOutput : String) =
    copy(expectedOutput = expectedOutput)
}