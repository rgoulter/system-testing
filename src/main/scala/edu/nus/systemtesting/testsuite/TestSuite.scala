package edu.nus.systemtesting.testsuite

import java.io.PrintWriter
import scala.collection.mutable.MutableList
import com.typesafe.config.Config
import edu.nus.systemtesting.HipTestCase
import edu.nus.systemtesting.HipTestCase.constructHipTestCase
import edu.nus.systemtesting.output.ConsoleOutputGenerator
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.TestPassed
import edu.nus.systemtesting.TestFailed
import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.FileSystemUtilities.getCurrentDateString
import edu.nus.systemtesting.output.ConsoleOutputGenerator

class TestSuite(writer : PrintWriter = new PrintWriter(System.out, true),
                configuration : Config,
                tests : List[TestCase])
    extends ConsoleOutputGenerator {
  def MILLI_CONVERSION_FACTOR = 1000
  val THRESHOLD = (configuration.getLong("SIGNIFICANT_TIME_THRESHOLD") * MILLI_CONVERSION_FACTOR)

  val successes = new MutableList[String]()
  val failures = new MutableList[String]()

  var performanceOutput = ""

  def runAllTests() : Unit = {
    val startTime = System.currentTimeMillis

    tests.foreach(test => {
      val testResult = test.generateOutput

      if (test.outputFileName.length > 0)
        writeToFile(test.outputFileName,
                    test.outputDirectory,
                    testResult.executionOutput.output)

      testResult.result match {
        case TestPassed => successes += test.fileName
        case TestFailed => failures += test.fileName
      }

      if (testResult.executionTime > THRESHOLD) {
        performanceOutput += testResult.filename + "\n"
        performanceOutput += "Runtime was " + testResult.executionTime + " milliseconds\n"
      }

      displayResult(testResult)
    })

    val endTime = System.currentTimeMillis

    val timeTaken = (endTime - startTime) / MILLI_CONVERSION_FACTOR

    writer.println(log(s"Total time taken to run all tests: $timeTaken seconds"))
    createPerformanceReport(performanceOutput, configuration, writeToFile)
  }

  def displayResult(result : TestCaseResult) = {
    // Assuming that execCmd is of same form as run in Runnable
    val execCmd = Seq(result.command, result.arguments, result.filename).mkString(" ")
    writer.println(execCmd)

    result.result match {
      case TestPassed => writer.println(passed)
      case TestFailed => writer.println(failed)
    }

    result.remarks.foreach(writer.println)

    val time = result.executionTime

    if (time > THRESHOLD) {
      writer.println("Runtime: " + time + " milliseconds")
    }
  }

  def generateTestStatistics() : Unit = {
    writer.println(log("Total number of tests: " + (successes.length + failures.length)))
    writer.println(success("Total number of tests passed: " + successes.length))
    writer.println(error("Total number of tests failed: " + failures.length))
  }


  def createPerformanceReport(performanceOutput : String,
                              configuration : Config,
                              writeToFile : (String, String, String, String) => Unit) : Unit = {
    val fileName = "sleek_performance_report_" + getCurrentDateString

    writeToFile(fileName, configuration.getString("SLEEK_OUTPUT_DIRECTORY"), performanceOutput, ".perf")
  }
}
