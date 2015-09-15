package edu.nus.systemtesting.testsuite

import java.io.PrintWriter
import scala.collection.mutable.MutableList
import scala.sys.process.stringToProcess

import edu.nus.systemtesting.{ Result, TestCase,
                               TestCaseResult, TestFailed, TestPassed }
import edu.nus.systemtesting.output.ConsoleOutputGenerator
import org.joda.time.DateTime

class TestSuite(tests: List[TestCase],
                revision: String,
                significantTime: Long,
                writer: PrintWriter = new PrintWriter(System.out, true))
    extends ConsoleOutputGenerator {
  // significantTime in seconds
  val THRESHOLD = (significantTime * 1000)

  def runAllTests(): TestSuiteResult = {
    val startTime = System.currentTimeMillis

    val testResults = tests.map(test => {
      val testResult = test.generateOutput

      displayResult(testResult)

      testResult
    })

    val endTime = System.currentTimeMillis

    val timeTaken = (endTime - startTime) / 1000

    writer.println(log(s"Total time taken to run all tests: $timeTaken seconds"))

    // assuming the `hostname` command can't/won't fail
    val hostname : String = "hostname" !!
    val now = DateTime.now()
    val suiteResult = TestSuiteResult(hostname, now, revision, testResults)

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
