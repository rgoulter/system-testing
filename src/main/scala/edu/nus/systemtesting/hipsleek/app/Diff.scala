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
  import runHipSleek.{ runAllTests, runHipTests, runSleekTests }


  /** Used for `diffSuiteResults`, to save typing / screen space. */
  type DiffableResults = List[(String, TestSuiteResult, TestSuiteResult)]

  /** For use with `diffSuiteResults`, for running just sleek results. */
  private[app] def sleekResultPairs(rev1: Commit, rev2: Commit):
      DiffableResults = {
    val oldRes = runSleekTests(rev1)
    val curRes = runSleekTests(rev2)

    List(("sleek", oldRes, curRes))
  }

  /** For use with `diffSuiteResults`, for running just hip results. */
  private[app] def hipResultPairs(rev1: Commit, rev2: Commit):
      DiffableResults = {
    val oldRes = runHipTests(rev1)
    val curRes = runHipTests(rev2)

    List(("hip", oldRes, curRes))
  }

  /**
   * For use with `diffSuiteResults`, for running both sleek, hip results.
   *
   * The way it is implemented, the output of `diffSuiteResults` won't combine
   * the diff results together, so sleek diff will be followed by hip diff.
   */
  private[app] def allResultPairs(rev1: Commit, rev2: Commit):
      DiffableResults = {
    val (oldSleekResults, oldHipResults) = runAllTests(rev1)
    val (curSleekResults, curHipResults) = runAllTests(rev2)

    List(("sleek", oldSleekResults, curSleekResults),
         ("hip",   oldHipResults, curHipResults))
  }

  /**
   * Run the given `resultsFor` function, apply to `TestSuiteComparison`.
   * Outputs the result of the comparison.
   */
  private[app] def diffSuiteResults(rev1: Commit,
                                    rev2: Commit,
                                    resultsFor: (Commit, Commit) => DiffableResults): List[TestSuiteComparison] = {
    val diffable = resultsFor(rev1, rev2)

    if (!diffable.isEmpty) {
      diffable map { case (name, oldTSRes, curTSRes) =>
        val diff = TestSuiteComparison(oldTSRes, curTSRes)

        diff.display(name)

        diff
      }
    } else {
      reporter.log(s"Results unavailable for one of $rev1 or $rev2")

      List()
    }
  }
}