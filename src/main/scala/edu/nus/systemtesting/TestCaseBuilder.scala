package edu.nus.systemtesting

import java.nio.file.{ Path, Paths }

trait ConstructTests[T <: TestCase] {
  implicit def constructTestCase(tcb: TestCaseBuilder): T
}

/**
 * Class to facilitate DSL-esque construction of test cases.
 */
case class TestCaseBuilder(val commandName: Path = Paths.get(""),
                           val fileName: Path = Paths.get(""),
                           val arguments: String = "",
                           val outputDirectory: Path = Paths.get(""),
                           val outputFileName: Path = Paths.get(""),
                           val expectedOutput: String = "",
                           val timeout: Int = 300) {
  //
  // Helper functions for DSL-esque construction of testcase.
  //

  def runCommand(commandName: Path) =
    copy(commandName = commandName)

  def onFile(fileName: Path) =
    copy(fileName = fileName)

  def withArguments(arguments: String) =
    copy(arguments = arguments)

  def checkAgainst(expectedOutput: String) =
    copy(expectedOutput = expectedOutput)

  def timeoutAfter(timeout: Int) =
    copy(timeout = timeout)
}
