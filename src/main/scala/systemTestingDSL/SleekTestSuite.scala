package systemTestingDSL

import scala.collection.mutable.MutableList

class SleekTestSuite {
  var tests = new MutableList[SleekTestCaseBuilder]()
  var results = new MutableList[String]()

  def addTest(
    commandName: String,
    fileName: String,
    arguments: String,
    outputDirectoryName: String,
    outputFileName: String,
    expectedOutput: String,
    regex: String = "Entail\\s.*.*Valid.*|.*Fail.*"): Unit = {
    tests +=
      new SleekTestCaseBuilder runCommand commandName onFile fileName withArguments arguments storeOutputInDirectory outputDirectoryName withOutputFileName
      outputFileName checkAgainst expectedOutput usingRegex regex
  }

  def runAllTests: Unit = {
    tests.foreach(test => {
      val result = test.build.generateOutput
      results += result
      println(result)
    })
  }

  def generateTestStatistics: Unit = {
    println("Total number of tests: " + results.length)
    var passed = 0;
    results.foreach(result => {
      if (result.equals("Passed"))
        passed = passed + 1
    })
    println("Total number of tests passed: " + passed)
    println("Total number of tests failed: " + (results.length - passed))

  }
}