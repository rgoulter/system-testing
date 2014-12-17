package systemTestingDSL

import scala.collection.mutable.MutableList

class RegressionTestSuite {
  var tests = new MutableList[RegressionTestCaseBuilder]()
  var successes = new MutableList[String]()
  var failures = new MutableList[String]()

  def addTest(
    commandName: String,
    fileName: String,
    arguments: String,
    outputDirectoryName: String,
    outputFileName: String,
    expectedOutput: String,
    regex: String = "Entail\\s.*.*Valid.*|.*Fail.*"): Unit = {
    tests +=
      new RegressionTestCaseBuilder runCommand commandName onFile fileName withArguments arguments storeOutputInDirectory outputDirectoryName withOutputFileName outputFileName
  }

  def runAllTests: Unit = {
    tests.foreach(test => {
      val result = test.build.generateTestResults
      result match {
        case "Passed" => successes += test.fileName
        case _ => failures += test.fileName
      }
      displayResult(result)
      println()
    })
  }

  def displayResult(result: String) = result match {
    case "Passed" => consoleUtilities.success("Passed")
    case _ => consoleUtilities.error("Failed")
  }

  def generateTestStatistics: Unit = {
    consoleUtilities.log("Total number of tests: " + (successes.length + failures.length))
    consoleUtilities.success("Total number of tests passed: " + successes.length)
    consoleUtilities.error("Total number of tests failed: " + failures.length)
  }
}
