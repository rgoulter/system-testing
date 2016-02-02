package edu.nus.systemtesting.hipsleek.app

import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.testsuite.TestSuiteResult

/**
 * @author richardg
 */
class Diff(config: AppConfig) {
  val runHipSleek = new RunHipSleek(config)
  import runHipSleek.{ runHipTests, runSleekTests }
  val validate = new Validate(config)
  import validate.runSleekValidateTests


  /** For use with `diffSuiteResults`, for running just sleek results. */
  private[app] def sleekResultPairs(rev1: Commit, rev2: Commit):
      Option[DiffableResults] = {
    try {
      val oldRes = runSleekTests(rev1)
      val curRes = runSleekTests(rev2)

      Some(("sleek", oldRes, curRes))
    } catch {
      case e: Throwable => None
    }
  }

  /** For use with `diffSuiteResults`, for running just hip results. */
  private[app] def hipResultPairs(rev1: Commit, rev2: Commit):
      Option[DiffableResults] = {
    try {
      val oldRes = runHipTests(rev1)
      val curRes = runHipTests(rev2)

      Some(("hip", oldRes, curRes))
    } catch {
      case e: Throwable => None
    }
  }

  private[app] def validateSleekResultPairs(rev1: Commit, rev2: Commit):
      Option[DiffableResults] = {
    try {
      val oldRes = runSleekValidateTests(rev1)
      val curRes = runSleekValidateTests(rev2)

      Some(("sleek-validate", oldRes, curRes))
    } catch {
      case e: Throwable => None
    }
  }

  /**
   * Run the given `resultsFor` function, apply to `TestSuiteComparison`.
   * Outputs the result of the comparison.
   */
  private[app] def diffSuiteResults(rev1: Commit,
                                    rev2: Commit,
                                    resultsFor: (Commit, Commit) => Option[DiffableResults]): Option[TestSuiteComparison] = {
    val diffable = resultsFor(rev1, rev2)

    if (!diffable.isEmpty) {
      diffable map { case (name, oldTSRes, curTSRes) =>
        val diff = TestSuiteComparison(name, oldTSRes, curTSRes)

        diff.display(name)

        diff
      }
    } else {
      reporter.log(s"Results unavailable for one of $rev1 or $rev2")

      None
    }
  }
}
