package edu.nus.systemtesting

trait ConstructTests[T <: TestCase] {
  implicit def constructTestCase(tcb: TestCaseBuilder): T
}

/**
 * Class to facilitate DSL-esque construction of test cases.
 */
case class TestCaseBuilder(val commandName: String = "",
                           val fileName: String = "",
                           val arguments: String = "",
                           val outputDirectory: String = "",
                           val outputFileName: String = "",
                           val expectedOutput: String = "",
                           val timeout: Int = 300) {
  //
  // Helper functions for DSL-esque construction of testcase.
  //

  def runCommand(commandName: String) =
    copy(commandName = commandName)

  def onFile(fileName: String) =
    copy(fileName = fileName)

  def withArguments(arguments: String) =
    copy(arguments = arguments)

  def checkAgainst(expectedOutput: String) =
    copy(expectedOutput = expectedOutput)

  def timeoutAfter(timeout: Int) =
    copy(timeout = timeout)
}
