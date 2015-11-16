package edu.nus.systemtesting.hipsleek.app

import edu.nus.systemtesting.hg.Branch
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.ANSIReporter
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.output.HTMLOutput
import edu.nus.systemtesting.output.VisibilityOptions
import org.joda.time.format.ISODateTimeFormat
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.Testable

/**
 * For representing status of the `default` branch.
 */
class DefaultBranchStatus(val branch: Branch,
                          val tsRes: List[(String, TestSuiteResult)]) {
}

class BranchStatus(val branch: Branch,
                   val tsCmp: List[TestSuiteComparison],
                   val bisectResults: List[(Testable, Commit)]) {
}

class RepoStatus(config: AppConfig) {
  val repo = new Repository(config.repoDirOrDie)

  // Don't run, but need ConfiguredMain to invoke diffs between commits
  val configuredMain = new ConfiguredMain(config)
  val diff = new Diff(config)
  import diff.{ allResultPairs, diffSuiteResults }
  val bisector = new Bisect(config)
  import bisector.bisect
  val runHipSleek = new RunHipSleek(config)
  import runHipSleek.runAllTests

  val MainBranch = "default"

  private def runDefaultBranch(): DefaultBranchStatus = {
    // Find default commit..
    val defaultB = new Branch(repo, MainBranch)

    // The 'status' of the default branch is by running the tests on the latest
    // commits.
    val (sleekTSR, hipTSR) = runAllTests(defaultB)
    new DefaultBranchStatus(defaultB, List(("sleek", sleekTSR), ("hip", hipTSR)))
  }

  private def runBranch(branchName: String): BranchStatus = {
    val branch = new Branch(repo, branchName)
    import branch.{ latestCommit, earliestCommit }

    // Find latest merge w/ default
    // Assumption: all branches forked (eventually) w/ ... `default`.
    // TODO: Or should this be commonAncestor with branched-from?
    // val latestMergeC = Repo.commonAncestor(latestCommit, defaultB)

    val resultPairs = allResultPairs(_, _)

    // reporter.header(s"Run Diff (${idx+1}/${recentBranches.length})")

    // Purpose of the diff(s) here is "did this branch break anything";
    // but commits may fail to build.
    // So, need to take some care.
    def diffResults(earlyCommit: Commit, laterCommit: Commit): List[TestSuiteComparison] = {
      // Base Case; need to be different commits..
      if (earlyCommit == laterCommit) {
        // Unable to find diff for this branch
        List()
      } else {
        try {
          diffSuiteResults(earlyCommit, laterCommit, resultPairs)
        } catch {
          case buildFailure: UnableToBuildException => {
            val failedCommit = buildFailure.rev

            // assume that we never run a diff on root commit of the repo.
            // also, just take the first parent commit.
            if (failedCommit == earlyCommit) {
              val nextC = earlyCommit.parents.head
              diffResults(nextC, laterCommit)
            } else if (failedCommit == laterCommit) {
              val nextC = laterCommit.parents.head
              diffResults(earlyCommit, laterCommit)
            } else {
              throw new IllegalStateException()
            }
          }
        }
      }
    }

    val diffs = diffResults(earliestCommit, latestCommit)

    //
    // Bisect
    // each of the { was working -> now failing } TestCases.
    //
    val bisectRes = diffs map { tsCmp =>
      val numBisects = tsCmp.usedToPass.length

      tsCmp.usedToPass.zipWithIndex map { case ((oldTC, _), idx) =>
        reporter.header(s"Running bisection for ${oldTC.cmdFnArgsKey} on branch ${branch.name}, (${idx+1}/$numBisects)")

        val bisectTC = configuredMain.recoverTestableFromTCR(oldTC)

        val firstFailingC = bisect(earliestCommit, latestCommit, bisectTC)

        (bisectTC, firstFailingC)
      }
    } flatten

    new BranchStatus(branch, diffs, bisectRes)
  }

  def runBranchStatus(branchName: String): Unit = {
    if (branchName == MainBranch) {
      val defaultSt = runDefaultBranch()

      // Dump branch statuses to HTML.
      HTMLOutput.dumpDefaultBranchStatus(defaultSt)
    } else {
      val branchSt = runBranch(branchName)

      // Dump branch statuses to HTML.
      HTMLOutput.dumpBranchStatus(branchSt)
    }
  }

  def runStatus(): Unit = {
    val t = repo.tip()
    println(s"Latest commit: ${t.revHash} (${t.age})")

    //
    // Summarise branches
    //
    reporter.header("Recent Branches")

    val recentBranches = repo.recentBranches()
    recentBranches.foreach { x =>
      branchInfo(x.branch)
    }

    println()
    println(recentBranches.length + " total")

    // Find default commit..
    val defaultB = new Branch(repo, MainBranch)

    // The 'status' of the default branch is by running the tests on the latest
    // commits.
    val (sleekTSR, hipTSR) = runAllTests(defaultB)
    val defaultStatus = new DefaultBranchStatus(defaultB, List(("sleek", sleekTSR), ("hip", hipTSR)))

    //
    // Run diffs
    //
    reporter.header("Running Diffs")

    val diffableBranches = recentBranches.filterNot(_.branch.name == MainBranch)

    // MAGIC: 'default' as the main development branch.
    val res = diffableBranches.zipWithIndex.map { case (c, idx) =>
      reporter.header(s"Run Branch (${idx+1}/${recentBranches.length})")

      val branch = c.branch
      runBranch(branch.name)
    }

    // Dump branch statuses to HTML.
    HTMLOutput.dumpRepoStatus(t, defaultStatus, res)
  }

  // outputs a one-line summary of a branch
  def branchInfo(b: Branch) {
    val bf = b.branchedFrom
    val bfStr = bf.map({ c => f"${c.branch.name}%15s ${c.revHash} ${c.age}" }).getOrElse("-")

    println(f"${b.name}%15s ${b.latestCommit.revHash} ${b.age}%-13s branched from $bfStr")
  }
}
