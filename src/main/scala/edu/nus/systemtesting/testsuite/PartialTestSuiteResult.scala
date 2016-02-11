package edu.nus.systemtesting.testsuite

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Allows evaluating a [[TestSuiteResult]] where any of the futures throws an
 * exception will throw an exception for the whole thing. This 'wraps' the
 * results, evaluating `results` to be only the ones which didn't throw an
 * exception.
 *
 * @author richardg
 */
class PartialTestSuiteResult(tsr: TestSuiteResult) 
    extends TestSuiteResult(tsr.hostname, tsr.datetime, tsr.repoRevision, tsr.resultFutures) {
  override lazy val results = resultFutures map { future =>
    try {
      Some(Await result (future, Duration.Inf))
    } catch {
      case e: Throwable => None
    }
  } flatten

  /**
   * @param threshold for [[TestCaseResult.displayResult]], in seconds.
   */
  override def displayResult(threshold: Long = 1L): Unit = {
    // if we must have this...
    results map { tcr =>
      tcr.displayResult(threshold)
    }

    generateTestStatistics()
  }
}