package edu.nus.systemtesting.testsuite

import java.io.PrintWriter
import scala.collection.mutable.MutableList
import com.typesafe.config.Config
import edu.nus.systemtesting.SleekTestCaseBuilder
import edu.nus.systemtesting.output.ConsoleOutputGenerator
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.TestPassed
import edu.nus.systemtesting.TestFailed

class SleekTestSuite(writer: PrintWriter = new PrintWriter(System.out, true),
  configuration: Config)
    extends TestSuite with ConsoleOutputGenerator with PerformanceMetricsGenerator {
  val tests = new MutableList[SleekTestCaseBuilder]()
  val successes = new MutableList[String]()
  val failures = new MutableList[String]()
  val THRESHOLD = (configuration.getLong("SIGNIFICANT_TIME_THRESHOLD") * MILLI_CONVERSION_FACTOR)

  var performanceOutput = ""

  val DEFAULT_TEST_REGEX =
    "Entail.*:\\s.*Valid.*|Entail.*:\\s.*Fail.*|Entailing lemma.*:*Valid.*|Entailing lemma.*:.*Fail.*"

  def addTest(commandName: String,
    fileName: String,
    arguments: String,
    outputDirectoryName: String,
    outputFileName: String,
    expectedOutput: String,
    regex: String = DEFAULT_TEST_REGEX): Unit = {
    tests +=
      (new SleekTestCaseBuilder runCommand commandName
        onFile fileName
        withArguments arguments
        storeOutputInDirectory outputDirectoryName
        withOutputFileName outputFileName
        checkAgainst expectedOutput
        usingRegex regex)
  }

  def runAllTests(): Unit = {
    val startTime = System.currentTimeMillis

    tests.foreach(test => {
      val testResult = test.build.generateOutput

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

  override def displayResult(result : TestCaseResult) = {
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

  def generateTestStatistics: Unit = {
    writer.println(log("Total number of tests: " + (successes.length + failures.length)))
    writer.println(success("Total number of tests passed: " + successes.length))
    writer.println(error("Total number of tests failed: " + failures.length))
  }
}
