package edu.nus.systemtesting.hipsleek.app

import java.lang.Math.ceil
import java.lang.Math.log

import edu.nus.systemtesting.ConstructTestCase
import edu.nus.systemtesting.ExpectsOutput
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hipsleek.BuildFailed
import edu.nus.systemtesting.hipsleek.BuildResult
import edu.nus.systemtesting.hipsleek.BuildTimedOut
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.ReporterColors
import GlobalReporter.reporter

/**
 * @author richardg
 */
class Bisect(config: AppConfig) extends UsesRepository(config) {
  val runUtils = new RunUtils(config)
  import runUtils.runTestsWith
  val runHipSleek = new RunHipSleek(config)
  import runHipSleek.runTestCaseForRevision


  /**
   * Convenience method, using TestCaseResult explicitly.
   * Infers a construct function, and recovers Testable with ExpectsOutput on its own.
   */
  private[app] def bisect(workingCommit: Commit,
                          failingCommit: Commit,
                          tcr: TestCaseResult): Commit = {
    val (bisectTC, construct) = recoverFromTCR(tcr)
    bisect(workingCommit, failingCommit, bisectTC, construct)
  }

  private[app] def bisect(workingCommit: Commit,
                          failingCommit: Commit,
                          tc: Testable with ExpectsOutput,
                          construct: ConstructTestCase): Commit = {
    import ReporterColors.{ ColorCyan, ColorMagenta }

    val results = config.defaultResultsArchive

    // Check that the given revisions to arg make sense
    val tcr1 = results.resultFor(workingCommit)(tc)
    val tcr2 = results.resultFor(failingCommit)(tc)
    assume(workingCommit != failingCommit, "Must be different commits")
    assume(!tcr1.isEmpty, "Must have result for " + workingCommit)
    assume(!tcr2.isEmpty, "Must have result for " + failingCommit)
    assume(tcr1.get.passed, s"Assumed $tc passes for $workingCommit")
    assume(!tcr2.get.passed, s"Assumed $tc fails for $failingCommit")

    val runTest: Commit => TestCaseResult =
      runHipSleek.runTest(tc, construct)(_)

    // Load all the results we have for the given testable.
//    val revResPairs = results resultsFor tc
//    val revs = revResPairs map { case (r,_) => r }

    // 'bisect' is only interesting if we have the 'latest' failing,
    // and some earlier commit failing

    // Bisect vars
    var rev1 = workingCommit
    var rev2 = failingCommit
    var revRange = repo.commitsInRange(rev1, rev2).toBuffer

    // Commits which we encounter which have failed
    val buildFailureCommits = scala.collection.mutable.Set[Commit]()

    // Load the set of all commits which have previously been recorded as
    // 'build failure'.
    // It's a Bad Idea to add these to `buildFailureCommits` now, since this
    // would affect the commits which get used for the bisection. That results
    // in the bisection taking *even longer*.
    val globalBuildFailureCommits = results.loadBuildFailureCommits().map { revHash =>
      new Commit(repo, revHash.trim())
    } toSet

    import Math.{ log, ceil }
    def revRangeLen = revRange.length
    def numSteps = ceil(log(revRangeLen) / log(2)) toInt

    while (revRangeLen > 2) {
      revRange = repo.commitsInRange(rev1, rev2).toBuffer
      revRange --= buildFailureCommits

      reporter.header(s"$revRangeLen commits in range. $numSteps steps remain.", ColorMagenta)

      val nextRevIdx = if (revRangeLen % 2 == 0) revRangeLen / 2 else (revRangeLen - 1) / 2
      val nextRev = revRange(nextRevIdx)

      if (globalBuildFailureCommits contains nextRev) {
        // Only at the time we would encounter a failing commit,
        // note that the commit fails (in this bisection), continue.
        buildFailureCommits += nextRev
      } else {
        try {
          val nextTCR = runTest(nextRev)

          // output result, right
          nextTCR.displayResult()

          if (nextTCR.passed) {
            rev1 = nextRev
          } else {
            rev2 = nextRev
          }
        } catch {
          case e: UnableToBuildException => {
            if (!e.timedOut) {
              // Failed to build is the only reason runTestsWith will return None
              println("Build failure.")

              // Exclude the current `nextRev` from nextRevIdx
              buildFailureCommits += nextRev
              results.addBuildFailureCommit(nextRev.revHash)
            } else {
              // Failed to build is the only reason runTestsWith will return None
              println("Build timed out. Ignoring...")

              // Do nothing, with the logic that maybe trying again will help.
              // TODO: 'build timed out' is nonsense.
            }
          }
        }
      }
    }

    reporter.header("Bisect Result", ColorCyan)
    println(s"Latest working commit: $rev1")
    println(s"Earliest failing commit: $rev2")

    // return first commit which fails
    rev2
  }
}
