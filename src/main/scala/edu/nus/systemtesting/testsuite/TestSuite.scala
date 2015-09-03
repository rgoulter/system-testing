package edu.nus.systemtesting.testsuite

trait TestSuite {
  def addTest(commandName: String,
    fileName: String,
    arguments: String,
    outputDirectoryName: String,
    outputFileName: String,
    expectedOutput: String,
    regex: String): Unit

  def runAllTests(): Unit

  def displayResult(result: String): Unit
}