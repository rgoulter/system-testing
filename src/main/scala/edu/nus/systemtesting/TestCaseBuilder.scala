package edu.nus.systemtesting

import java.nio.file.{ Path, Paths }

/**
 * Class to facilitate DSL-esque construction of test cases.
 */
case class PreparedSystem(val binDir: Path = Paths.get(""),
                          val corpusDir: Path = Paths.get("")) {
  def withBinaries(binDir: Path) =
    copy(binDir = binDir)

  def withCorpus(corpusDir: Path) =
    copy(corpusDir = corpusDir)
}

/**
 * Class to facilitate DSL-esque construction of test cases.
 */
case class Testable(val commandName: Path = Paths.get(""),
                    val fileName: Path = Paths.get(""),
                    val arguments: String = "",
                    val expectedOutput: String = "") {
  def runCommand(commandName: Path) =
    copy(commandName = commandName)

  def onFile(fileName: Path) =
    copy(fileName = fileName)

  def withArguments(arguments: String) =
    copy(arguments = arguments)

  def checkAgainst(expectedOutput: String) =
    copy(expectedOutput = expectedOutput)
}

/**
 * Class to facilitate DSL-esque construction of test cases.
 */
case class TestCaseConfiguration(val timeout: Int = 300) {
  def timeoutAfter(timeout: Int) =
    copy(timeout = timeout)
}
