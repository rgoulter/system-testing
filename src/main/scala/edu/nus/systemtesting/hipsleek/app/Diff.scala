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
      DiffableResults = {
    val oldRes = runSleekTests(rev1)
    val curRes = runSleekTests(rev2)

    ("sleek", oldRes, curRes)
  }

  /** For use with `diffSuiteResults`, for running just hip results. */
  private[app] def hipResultPairs(rev1: Commit, rev2: Commit):
      DiffableResults = {
    val oldRes = runHipTests(rev1)
    val curRes = runHipTests(rev2)

    ("hip", oldRes, curRes)
  }

  private[app] def validateSleekResultPairs(rev1: Commit, rev2: Commit):
      DiffableResults = {
    val oldRes = runSleekValidateTests(rev1)
    val curRes = runSleekValidateTests(rev2)

    ("sleek-validate", oldRes, curRes)
  }

  /**
   * Run the given `resultsFor` function, apply to `TestSuiteComparison`.
   * Outputs the result of the comparison.
   *
   * May throw UnableToBuildException, if resultsFor throws this for either commit.
   */
  private[app] def diffSuiteResultsOnce(rev1: Commit,
                                        rev2: Commit,
                                        resultsFor: (Commit, Commit) => DiffableResults):
      TestSuiteComparison = {
    val (name, oldTSRes, curTSRes) = resultsFor(rev1, rev2)
    val diff = TestSuiteComparison(name, oldTSRes, curTSRes)

    diff.display(name)

    diff
  }

  /**
   * Best-effort to get some kind of comparison.
   * If the diff fails for some commit, diffSuiteResults is called
   * for the (first) parent of that commit.
   */
  def diffSuiteResults(earlyCommit: Commit, laterCommit: Commit, resultPairs: (Commit, Commit) => DiffableResults): Option[TestSuiteComparison] = {
    // Base Case; need to be different commits..
    if (earlyCommit == laterCommit) {
      // Unable to find diff for this branch
      None
    } else {
      try {
        Some(diffSuiteResultsOnce(earlyCommit, laterCommit, resultPairs))
      } catch {
        case buildFailure: UnableToBuildException => {
          val failedCommit = buildFailure.rev

          // Since the "Resolution Strategy" here is
          // "Commit Failed => Try its Parent Commit",
          // assume that we never run a diff on root commit of the repo.
          // also, just take the first parent commit.
          if (failedCommit == earlyCommit) {
            val nextC = earlyCommit.parents.head
            diffSuiteResults(nextC, laterCommit, resultPairs)
          } else if (failedCommit == laterCommit) {
            val nextC = laterCommit.parents.head
            diffSuiteResults(earlyCommit, nextC, resultPairs)
          } else {
            throw new IllegalStateException()
          }
        }
      }
    }
  }
}
