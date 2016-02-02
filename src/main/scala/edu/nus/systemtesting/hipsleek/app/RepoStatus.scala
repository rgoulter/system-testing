package edu.nus.systemtesting.hipsleek.app

import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.hg.Branch
import edu.nus.systemtesting.hg.Branch.branchToCommit
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.output.HTMLOutput
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.testsuite.TestSuiteResult

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

class RepoStatus(config: AppConfig) extends UsesRepository(config) {
  val diff = new Diff(config)
  import diff.{ sleekResultPairs, hipResultPairs, validateSleekResultPairs, diffSuiteResults }
  val bisector = new Bisect(config)
  import bisector.bisect
  val runHipSleek = new RunHipSleek(config)
  import runHipSleek.{ runSleekTests, runHipTests }

  // MAGIC: 'default' as the main development branch.
  val MainBranch = "default"

  private def runDefaultBranch(): DefaultBranchStatus = {
    // Find default commit..
    val defaultB = new Branch(repo, MainBranch)

    // The 'status' of the default branch is by running the tests on the latest
    // commits.
    val sleekTSR = runSleekTests(defaultB)
    val hipTSR = runHipTests(defaultB)
    new DefaultBranchStatus(defaultB, List(("sleek", sleekTSR), ("hip", hipTSR)))
  }

  private def runBranch(branchName: String): BranchStatus = {
    val branch = new Branch(repo, branchName)
    import branch.{ latestCommit, earliestCommit }

    // Find latest merge w/ default
    // Assumption: all branches forked (eventually) w/ ... `default`.
    // TODO: Or should this be commonAncestor with branched-from?
    // val latestMergeC = Repo.commonAncestor(latestCommit, defaultB)

    val configHasValidate = config.commands contains SleekConfigArg(isValidate = true)

    val resultPairs =
      if (!configHasValidate)
        // XXX Broken! Want *both* sleek, hip.
        // The intention of "all" is "do sleek, followed by hip stuff".
        sleekResultPairs(_, _)
      else
        validateSleekResultPairs(_, _)


    // reporter.header(s"Run Diff (${idx+1}/${recentBranches.length})")

    // Purpose of the diff(s) here is "did this branch break anything";
    // but commits may fail to build.
    // So, need to take some care.
    def diffResults(earlyCommit: Commit, laterCommit: Commit): Option[TestSuiteComparison] = {
      // Base Case; need to be different commits..
      if (earlyCommit == laterCommit) {
        // Unable to find diff for this branch
        None
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
              diffResults(earlyCommit, nextC)
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

      tsCmp.usedToPass.zipWithIndex map { case ((oldTCR, _), idx) =>
        reporter.header(s"Running bisection for ${oldTCR.cmdFnArgsKey} on branch ${branch.name}, (${idx+1}/$numBisects)")

        val firstFailingC = bisect(earliestCommit, latestCommit, oldTCR)

        (oldTCR, firstFailingC)
      }
    } getOrElse List()

    new BranchStatus(branch, diffs.toList, bisectRes)
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
    val sleekTSR = runSleekTests(defaultB)
    val hipTSR = runHipTests(defaultB)
    val defaultStatus = new DefaultBranchStatus(defaultB, List(("sleek", sleekTSR), ("hip", hipTSR)))

    //
    // Run diffs
    //
    reporter.header("Running Diffs")

    val diffableBranches = recentBranches.filterNot(_.branch.name == MainBranch)

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
