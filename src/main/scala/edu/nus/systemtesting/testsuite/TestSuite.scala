package edu.nus.systemtesting.testsuite

import edu.nus.systemtesting.TestCaseResult

trait TestSuite {
  def addTest(commandName : String,
              fileName : String,
              arguments : String,
              outputDirectoryName : String,
              outputFileName : String,
              expectedOutput : String) : Unit

  def runAllTests() : Unit

  def displayResult(result : TestCaseResult) : Unit
}
