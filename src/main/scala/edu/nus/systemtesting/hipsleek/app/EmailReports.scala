package edu.nus.systemtesting.hipsleek.app

import edu.nus.systemtesting.hg.{ Branch, Commit, Repository }
import edu.nus.systemtesting.hipsleek.{ HipTestSuiteUsage, SleekTestSuiteUsage }
import edu.nus.systemtesting.testsuite.{ TestSuiteResult, TestSuiteComparison }
import edu.nus.systemtesting.serialisation.ResultsArchive
import edu.nus.systemtesting.{ ExpectsOutput, Testable }


/**
 * @author richardg
 */
object EmailReports {
  // get which branches have been updated in this time; in terms of sequences of commits...
  def branchesUpdatedInRange(repo: Repository, from: Commit, to: Commit): List[(Branch, Seq[Commit])] = {
    val commitsInRange = repo.commitsBetween(from, to)

    commitsInRange.groupBy(_.branch).toList
  }

  // TODO: This was adapted from RunFast; extract-out the function, somewhere.
  private def allTestableForSuite(suite: Suite)(implicit validate: Validate): List[Testable with ExpectsOutput] =
    suite match {
      case HipOnly()           => HipTestSuiteUsage.allTestable
      case SleekOnly()         => SleekTestSuiteUsage.allTestable
      case All()               => SleekTestSuiteUsage.allTestable ++ HipTestSuiteUsage.allTestable
      case SleekValidateOnly() => validate.allTestable
    }
}

class EmailReports(config: AppConfig, suite: Suite) extends UsesRepository(config) {
  // atm, more of a dry-run for these things.
  def run(): Unit = {
    println("RUNNING SEND EMAIL STATUS")

    // infer from "last-read".
    // .. or, by default, "one week ago", say?
    // In lieu of that, what are some good values to test with??
    val fromC = repo.identify(Some("1a1b6c"))

    // by-default, the repo's tip.
    val toC   = repo.identify(Some("4b36a"))

    val modifiedBranches = EmailReports.branchesUpdatedInRange(repo, fromC, toC)

    println(s"${modifiedBranches.length} branches modified in range $fromC ... $toC")

    modifiedBranches foreach { case (branch, commits) =>
      println(s"Modified Branch $branch")

      val contributors = commits.map(c => c.email.takeWhile(x => x != '@')).toSet
      println("Contributors:")
      contributors.foreach(c => println(s"* $c"))

      val ResArch = config.defaultResultsArchive

      println("Loading Results...")
      val resultComparisons = resultPairsForBranch(ResArch, commits)

      // XXX: Generate HTML (or plaintext?) from these comparisons.
      println("Generating HTML...")

      // XXX Lookup contributor->email, for who to email.

      // XXX Send email

      println()
    }
  }

  // get the (from, to) result pairs for some segment of commits.
  def resultPairsForBranch(resArch: ResultsArchive, domain: Seq[Commit]): List[TestSuiteComparison] = {
    implicit val validate = new Validate(config)
    val testables: List[Testable] = EmailReports.allTestableForSuite(suite)

    // Now, intersect which has results
    val commitsWithResults = domain filter { commit =>
      val resultsForCommit = testables flatMap { testable =>
        resArch.resultFor(commit)(testable)
      }

      // We want 'earliest, latest commits which have the results for the suite'.
      // Demanding *all* testable of suite is somewhat inflexible for e.g. Validateable Testables.
      // Instead, we assume that this EmailReports computation is called on a range where the
      // earliest, latest actually have 'all' the results for the testables of the Suite.

      !resultsForCommit.isEmpty
    }

    assume(commitsWithResults.length >= 2)

    val oldRev = commitsWithResults.head
    val curRev = commitsWithResults.last

    // TestSuiteResult not obtained from results archive, but from 'running' the suite at that commit.
    // Since we (assume we) have results, this shouldn't take very long to 'run':
    // (also, Validate is used to get TSR for that commit. So).

    val diff = new Diff(config)
    import diff.{ DiffableResults, allResultPairs, hipResultPairs, sleekResultPairs, diffSuiteResults, validateSleekResultPairs }

    // TODO: This snippet of code is duplicated from Main; depends on a Diff object, though.
    val resultPairs: (Commit, Commit) => DiffableResults =
      config.runCommand match {
        case All()               => allResultPairs
        case SleekOnly()         => sleekResultPairs
        case HipOnly()           => hipResultPairs
        case SleekValidateOnly() => validateSleekResultPairs
        case _ =>
          throw new IllegalStateException
      }

    diff.diffSuiteResults(oldRev, curRev, ???)
  }

  // construct email/report using (from, to) result pairs.
  // OR WHATEVER THE FUCKING RETURN TYPE IS. FOR FUCK'S SAKE.

  // send an email...
}