package edu.nus.systemtesting.hipsleek.app

import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.ANSIReporter
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.output.VisibilityOptions
import org.joda.time.format.ISODateTimeFormat

/**
 * @author richardg
 */
object RepoStatus {
  def main(args: Array[String]): Unit = {
    val appCfg = Main.loadConfig()

    // Override options from loaded config with CLAs,
    // run with configuration.
    AppConfig.CommandLineOptionsParser.parse(args, appCfg) foreach { config =>
      // Configure output visibility
      import config.outputVis
      import VisibilityOptions.ShowANSI

      outputVis.when(ShowANSI) {
        GlobalReporter.reporter = new ANSIReporter()
      }

      GlobalReporter.visibility = outputVis

      val configuredStatus = new ConfiguredRepoStatus(config)
      configuredStatus.runStatus()
    }
  }
}

class ConfiguredRepoStatus(config: AppConfig) {
  val repoDir = config.repoDirOrDie
  val Repo = new Repository(repoDir)

  // Don't run, but need ConfiguredMain to invoke diffs between commits
  val configuredMain = new ConfiguredMain(config)

  def runStatus(): Unit = {
    val t = Repo.tip()
    println(s"Latest commit: ${t.revHash} (${t.age})")

    //
    // Summarise branches
    //
    reporter.header("Recent Branches")

    val recentBr = Repo.recentBranches()
    recentBr.foreach { x =>
      branchInfo(x.branch)
    }

    println()
    println(recentBr.length + " total")

    // Find default commit..
    // MAGIC: 'default' as the main development branch.
    val defaultB = new Repo.Branch("default")

    //
    // Run diffs
    //
    reporter.header("Running Diffs")
    // MAGIC: 'default' as the main development branch.
    recentBr.filterNot(_.branch.name == "default").zipWithIndex.foreach { case (c, idx) =>
      val branch = c.branch
      import branch.{ latestCommit, earliestCommit }

      // Find latest merge w/ default
      // Assumption: all branches forked (eventually) w/ ... `default`.
      // TODO: Or should this be commonAncestor with branched-from?
      // val latestMergeC = Repo.commonAncestor(latestCommit, defaultB)

      // XXX: resultPairs should be 'all'.
      // unlikely to want 'status report' of *just* the Sleek ones, right?
      val resultPairs = configuredMain.sleekResultPairs(_, _)

      // using instance-based Commit objects was a bad idea;
      // quick Hack to get around that.
      val oldestC = new configuredMain.Repo.Commit(earliestCommit.revHash)
      val newestC = new configuredMain.Repo.Commit(latestCommit.revHash)

      reporter.header(s"Run Diff (${idx+1}/${recentBr.length})")

      configuredMain.diffSuiteResults(oldestC, newestC, resultPairs)
    }
  }

  // outputs a one-line summary of a branch
  def branchInfo(b: Repo.Branch) {
    val bf = b.branchedFrom
    val bfStr = bf.map({ c => f"${c.branch.name}%15s ${c.revHash} ${c.age}" }).getOrElse("-")

    println(f"${b.name}%15s ${b.latestCommit.revHash} ${b.age}%-13s branched from $bfStr")
  }
}