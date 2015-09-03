package edu.nus.systemtesting.testsuite

import java.io.PrintWriter

import scala.collection.mutable.MutableList

import com.typesafe.config.Config

import edu.nus.systemtesting.HipTestCaseBuilder
import edu.nus.systemtesting.output.ConsoleOutputGenerator

case class HipTestSuite(writer: PrintWriter = new PrintWriter(System.out, true),
  configuration: Config)
    extends TestSuite with ConsoleOutputGenerator with PerformanceMetricsGenerator {
  var tests = new MutableList[HipTestCaseBuilder]()
  var successes = new MutableList[String]()
  var failures = new MutableList[String]()
  var THRESHOLD = (configuration.getLong("SIGNIFICANT_TIME_THRESHOLD") * MILLI_CONVERSION_FACTOR)
  var performanceOutput = ""

  def addTest(commandName: String,
    fileName: String,
    arguments: String,
    outputDirectoryName: String,
    outputFileName: String,
    expectedOutput: String,
    regex: String = "Procedure.*FAIL.*|Procedure.*SUCCESS.*"): Unit = {
    tests +=
      (new HipTestCaseBuilder runCommand commandName
        onFile fileName
        withArguments arguments
        storeOutputInDirectory outputDirectoryName
        withOutputFileName outputFileName
        checkAgainst expectedOutput
        usingRegex regex)
  }

  def runAllTests(): Unit = {
    var startTime = System.currentTimeMillis

    tests.foreach(test => {
      lazy val (err, passOrFail, time) = test.build.generateOutput

      passOrFail match {
        case "Passed" => successes += test.fileName
        case _ => failures += test.fileName
      }

      displayResult(passOrFail)

      if (err.isDefined)
        writer.println(err.get)

      if (time > THRESHOLD) {
        performanceOutput += test.fileName + "\n" + "Runtime was " + time + " milliseconds \n"
        writer.println("Runtime: " + time + "milliseconds")
      }
    })

    var endTime = System.currentTimeMillis

    val timeTaken = (endTime - startTime)

    writer.println(log(s"Total time taken to run all tests: $timeTaken seconds"))
    createPerformanceReport(performanceOutput, configuration, writeToFile)
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