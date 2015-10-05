package edu.nus.systemtesting.testsuite

import scala.concurrent.{ Await, Future, promise, future }
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext
import scala.collection.mutable.MutableList
import scala.sys.process.stringToProcess
import org.joda.time.DateTime
import edu.nus.systemtesting.{ Result, TestCase,
                               TestCaseResult, TestFailed, TestPassed }
import edu.nus.systemtesting.output.GlobalReporter
import GlobalReporter.reporter
import edu.nus.systemtesting.serialisation.ResultsArchive

class TestSuite(tests: List[TestCase],
                revision: String,
                significantTime: Long) {
  def runAllTests(resultsArch: ResultsArchive): TestSuiteResult = {
    // Use global ExecutionContext for executing context.
    import ExecutionContext.Implicits.global

    val tcPromises = tests map { tc =>
      (tc, promise[TestCaseResult])
    }
    val tcFutures = tcPromises map { case (tc, p) => p.future }

    // Asynchronously, run each test case (synchronously),
    // so that `TestSuite.runAllTests()` returns, but TestCases don't
    // necessarily have results yet.
    val fut = Future {
      val startTime = System.currentTimeMillis

      tcPromises foreach { case (tc, p) =>
        // Load or run each result individually.
        // May be worth recording how many loaded vs computed??..
        val testResult = resultsArch.resultFor(revision)(tc) match {
          case Some(tcr) => tcr

          case None => {
            val tcr = tc.generateOutput

            // Didn't have results, save them.
            try {
              resultsArch.saveTestCaseResult(revision, tcr)
            } catch {
              case e: Throwable => {
                e.printStackTrace()
              }
            }

            tcr
          }
        }

        // the promise always succeeds.
        // (not to be confused with the result of the test; even
        //  if test is 'invalid', don't model that using promise failure).
        p success testResult
      }

      val endTime = System.currentTimeMillis

      val timeTaken = (endTime - startTime) / 1000

      // *might* interleave with other output, if not careful.. :/
      // but this also isn't too important, so.
      // (if output does screw up, it's cheap to replay).
      reporter.println()
      reporter.log(s"Total time taken to run all tests: $timeTaken seconds")
      reporter.println()
    }

    fut onFailure { case e: Throwable =>
      System.err.println("Something went wrong in future!!!")
      e.printStackTrace()
    }

    // assuming the `hostname` command can't/won't fail
    val hostname : String = ("hostname" !!).trim()
    val now = DateTime.now()
    val suiteResult = TestSuiteResult(hostname, now, revision, tcFutures)

    suiteResult
  }
}
