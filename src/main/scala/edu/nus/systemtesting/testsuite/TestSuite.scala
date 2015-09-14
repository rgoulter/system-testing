package edu.nus.systemtesting.testsuite

import java.io.PrintWriter
import scala.collection.mutable.MutableList
import scala.sys.process.stringToProcess

import com.typesafe.config.Config
import edu.nus.systemtesting.{ Result, SystemPreparation, TestCase,
                               TestCaseResult, TestFailed, TestPassed }
import edu.nus.systemtesting.output.ConsoleOutputGenerator
import org.joda.time.DateTime

class TestSuite(configuration: Config,
                tests: List[TestCase],
                writer: PrintWriter = new PrintWriter(System.out, true))
    extends ConsoleOutputGenerator {
  def MILLI_CONVERSION_FACTOR = 1000
  val THRESHOLD = (configuration.getLong("SIGNIFICANT_TIME_THRESHOLD") * MILLI_CONVERSION_FACTOR)

  def runAllTests(): TestSuiteResult = {
    val startTime = System.currentTimeMillis

    val testResults = tests.map(test => {
      val testResult = test.generateOutput

      displayResult(testResult)

      testResult
    })

    val endTime = System.currentTimeMillis

    val timeTaken = (endTime - startTime) / MILLI_CONVERSION_FACTOR

    writer.println(log(s"Total time taken to run all tests: $timeTaken seconds"))

    // assuming the `hostname` command can't/won't fail
    val hostname : String = "hostname" !!
    val now = DateTime.now()
    val suiteResult = TestSuiteResult(hostname, now, "reporevision", testResults)

    suiteResult
  }

  def displayResult(result: TestCaseResult) = {
    // Assuming that execCmd is of same form as run in Runnable
    val execCmd = Seq(result.command, result.arguments, result.filename).mkString(" ")
    writer.println(execCmd)

    result.result match {
      case TestPassed => writer.println(passed)
      case TestFailed => writer.println(failed)
    }

    result.results match {
      case Left(remarks) => {
        remarks.foreach(writer.println)
      }
      case Right(results) => {
        val diff = results.filterNot(_.passed)

        diff.foreach({ case Result(key, expected, got) =>
          writer.println(s"Expected ${expect(expected)}, but got ${actual(got)} for $key")
        })
      }
    }

    writer.println


    val time = result.executionTime

    if (time > THRESHOLD) {
      writer.println("Runtime: " + time + " milliseconds")
    }

    writer.println
  }
}
