package systemTestingDSL

import scala.collection.mutable.MutableList
import systemTestingDSL.outputGenerator.ConsoleOutputGenerator

class RegressionTestSuite extends ConsoleOutputGenerator {
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
    case "Passed" => passed
    case _ => failed
  }

  def generateTestStatistics: Unit = {
    log("Total number of tests: " + (successes.length + failures.length))
    success("Total number of tests passed: " + successes.length)
    error("Total number of tests failed: " + failures.length)
  }
}
