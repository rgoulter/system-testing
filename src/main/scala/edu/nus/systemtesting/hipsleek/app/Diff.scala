package edu.nus.systemtesting.hipsleek.app

import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.testsuite.TestSuiteResult

object Diff {
  /**
   * Curry-able function, to abstract e.g. sleekResultPairs.
   */
  def resultPairsFor(name: String, suiteResultFor: Commit => TestSuiteResult)
                    (rev1: Commit, rev2: Commit): DiffableResults = {
    val oldRes = suiteResultFor(rev1)
    val curRes = suiteResultFor(rev2)

    (name, oldRes, curRes)
  }
}

/**
 * @author richardg
 */
class Diff(config: AppConfig) {
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
    def retry(failedCommit: Commit): Option[TestSuiteComparison] = {
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

    // Base Case; need to be different commits..
    if (earlyCommit == laterCommit) {
      // Unable to find diff for this branch
      None
    } else {
      try {
        Some(diffSuiteResultsOnce(earlyCommit, laterCommit, resultPairs))
      } catch {
        case buildFailure: UnableToBuildException => retry(buildFailure.rev)
        case runFailure: UnableToRunException => retry(runFailure.rev)
      }
    }
  }
}
