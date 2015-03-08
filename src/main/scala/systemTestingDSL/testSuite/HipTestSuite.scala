package systemTestingDSL.testSuite

import java.io.PrintWriter
import scala.collection.mutable.MutableList
import systemTestingDSL.HipTestCaseBuilder
import systemTestingDSL.outputGenerator.ConsoleOutputGenerator

case class HipTestSuite(writer: PrintWriter = new PrintWriter(System.out, true)) extends ConsoleOutputGenerator {
  var tests = new MutableList[HipTestCaseBuilder]()
  var successes = new MutableList[String]()
  var failures = new MutableList[String]()

  def addTest(commandName: String,
    fileName: String,
    arguments: String,
    outputDirectoryName: String,
    outputFileName: String,
    expectedOutput: String,
    regex: String = "Procedure.*FAIL.*|Procedure.*SUCCESS.*"): Unit = {
    tests +=
      new HipTestCaseBuilder runCommand commandName onFile fileName withArguments arguments storeOutputInDirectory outputDirectoryName withOutputFileName
      outputFileName checkAgainst expectedOutput usingRegex regex
  }

  def runAllTests(): Unit = {
    tests.foreach(test => {
      lazy val result = test.build.generateOutput
      result._2 match {
        case "Passed" => successes += test.fileName
        case _ => failures += test.fileName
      }
      displayResult(result._2)
      if (result._1.isDefined)
        writer.println(result._1.get)
      writer.println
    })
  }
  def displayResult(result: String) = result match {
    case "Passed" => println(passed)
    case _ => println(failed)
  }

  def generateTestStatistics: Unit = {
    writer.println(log("Total number of tests: " + (successes.length + failures.length)))
    writer.println(success("Total number of tests passed: " + successes.length))
    writer.println(error("Total number of tests failed: " + failures.length))
  }
}