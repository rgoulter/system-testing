package systemTestingDSL.testSuite

import scala.collection.mutable.MutableList
import systemTestingDSL.outputGenerator.ConsoleOutputGenerator
import java.io.PrintWriter
import systemTestingDSL.SleekTestCaseBuilder
import com.typesafe.config.Config
import systemTestingDSL.FileSystemUtilities

class SleekTestSuite(writer: PrintWriter = new PrintWriter(System.out, true), configuration: Config) extends TestSuite with ConsoleOutputGenerator with PerformanceMetricsGenerator {
  var tests = new MutableList[SleekTestCaseBuilder]()
  var successes = new MutableList[String]()
  var failures = new MutableList[String]()
  var THRESHOLD = (configuration.getLong("SIGNIFICANT_TIME_THRESHOLD") * MILLI_CONVERSION_FACTOR)
  var performanceOutput = ""

  def addTest(
    commandName: String,
    fileName: String,
    arguments: String,
    outputDirectoryName: String,
    outputFileName: String,
    expectedOutput: String,
    regex: String = "Entail.*:\\s.*Valid.*|Entail.*:\\s.*Fail.*|Entailing lemma.*:*Valid.*|Entailing lemma.*:.*Fail.*"): Unit = {
    tests +=
      new SleekTestCaseBuilder runCommand commandName onFile fileName withArguments arguments storeOutputInDirectory outputDirectoryName withOutputFileName
      outputFileName checkAgainst expectedOutput usingRegex regex
  }

  def runAllTests(): Unit = {
    var startTime = System.currentTimeMillis
    tests.foreach(test => {
      lazy val result = test.build.generateOutput
      result._2 match {
        case "Passed" => successes += test.fileName
        case _ => failures += test.fileName
      }
      displayResult(result._2)
      if (result._1.isDefined)
        writer.println(result._1.get)
      if (result._3 > THRESHOLD) {
        performanceOutput += test.fileName + "\n" + "Runtime was " + result._3 + " milliseconds \n"
        writer.println("Runtime: " + result._3 + "milliseconds")
      }
    })
    var endTime = System.currentTimeMillis
    val timeTaken = (endTime - startTime) / MILLI_CONVERSION_FACTOR
    writer.println(log(s"Total time taken to run all tests: $timeTaken seconds"))
    createPerformanceReport(performanceOutput, configuration, writeToFile)
  }

  def displayResult(result: String) = result match {
    case "Passed" => writer.println(passed)
    case _ => writer.println(failed)
  }

  def generateTestStatistics: Unit = {
    writer.println(log("Total number of tests: " + (successes.length + failures.length)))
    writer.println(success("Total number of tests passed: " + successes.length))
    writer.println(error("Total number of tests failed: " + failures.length))
  }
}
