package edu.nus.systemtesting.hipsleek.app

import java.nio.file.Path
import java.nio.file.Paths

import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigFactory

import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.ANSIReporter
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.output.VisibilityOptions
import edu.nus.systemtesting.serialisation.ResultsArchive

class UnableToBuildException(repoDir: Path,
                             val rev: Commit,
                             val timedOut: Boolean = false)
  extends RuntimeException(s"Cannot build for revision ${rev.revHash} in dir $repoDir")

object Main {
  /** Expected filename for the application conf. */
  val ConfigFilename = ".hipsleektest.conf"

  private def pathAncestors(p: Path): List[Path] = {
      val par = p.getParent()

      if (par == null)
        List()
      else
        par +: pathAncestors(par)
    }

  private val cwdPath = Paths.get(".").toAbsolutePath()
  private val ancestors = cwdPath +: pathAncestors(cwdPath)

  /*
   * Try to find some Repository path.
   *
   * check for a `.hg` folder in CWD,
   * or an ancestor of this. Assume that this is the hip/sleek repository.
   *
   * Load config, maybe from Current Working Directory, or some ancestor of this,
   * or from `$HOME`.
   *
   * Failing that, load the `application.conf` from the JAR's resources.
   */
  def findRepository(): Option[Path] = {
    // Look for .hg in cwd + ancestors.
    val maybeRepo = ancestors find { path => (path resolve ".hg") toFile() exists() }

    if (!maybeRepo.isEmpty) {
      reporter.log(s"Found HG repository at ${maybeRepo.getOrElse("")}.")

      maybeRepo
    } else {
      // Look for configs in cwd + ancestors.
      val maybeConfig = ancestors map { _ resolve ConfigFilename } filter { _ toFile() exists }

      maybeConfig.map({ path =>
        // Loads *every* config in the ancestors, rather than until we find one
        // with a REPO_DIR key.
        reporter.log(s"Found ${path.normalize()}. Loading Config.")

        try {
          val cfg = ConfigFactory.parseFile(path.toFile())
          val repoDir = cfg.getString("REPO_DIR")

          Some(Paths.get(repoDir))
        } catch {
          case missing: ConfigException.Missing => None
          case _: Throwable => None
        }
      }) find { maybeRepo => !maybeRepo.isEmpty } flatten
    }
  }

  /**
   * Loads the config,
   */
  def loadConfig(): AppConfig = {
    // Look for first config in cwd + ancestors.
    val maybeConfig = ancestors map { _ resolve ConfigFilename } find { _ toFile() exists }

    maybeConfig match {
      case Some(path) =>
        // assumes that the file will load okay,
        // otherwise will throw nasty exception.
        // TODO: A bit tedious to deal with this; but shouldn't be a common problem
        AppConfig.load(ConfigFactory.parseFile(path.toFile()),
                       maybeRepoDir = findRepository())
      case None =>
        AppConfig.load(maybeRepoDir = findRepository())
    }
  }

  def main(args: Array[String]): Unit = {
    val appCfg = loadConfig()

    // Override options from loaded config with CLAs,
    // run with configuration.
    AppConfig.CommandLineOptionsParser.parse(args, appCfg) foreach { config =>
      val configuredMain = new ConfiguredMain(config)

      // Configure output visibility
      import config.outputVis
      import VisibilityOptions.ShowANSI

      outputVis.when(ShowANSI) {
        GlobalReporter.reporter = new ANSIReporter()
      }

      GlobalReporter.visibility = outputVis

      configuredMain.run()
    }
  }
}

/**
 * Convenience class so that the logic for running this program can access an
 * immutable configuration.
 */
class ConfiguredMain(config: AppConfig) {
  // Each instance of `ConfiguredMain` only ever uses the one `Repository`
  val repo = new Repository(config.repoDirOrDie)


  // Easier to split logic-heavy parts of main into other classes
  val runUtils = new RunUtils(config)
  import runUtils.runTestsWith
  val runHipSleek = new RunHipSleek(config)
  import runHipSleek.{ runAllTests, runHipTests, runSleekTests }
  val diff = new Diff(config)
  import diff.{ DiffableResults, allResultPairs, hipResultPairs, sleekResultPairs, diffSuiteResults }
  val bisector = new Bisect(config)
  import bisector.bisect


  private[hipsleek] def run(): Unit = {
    import config.{ command, rev }

    val revision = repo.identify(rev)

    command match {
      case "sleek"  => runSleekTests(revision)
      case "hip"    => runHipTests(revision)
      case "all"    => runAllTests(revision)
      case "diff"   => runSuiteDiff(config.rev1, config.rev2)
      case "bisect" => runBisect()
      case "status-repo" => {
        val repoStatus = new RepoStatus(config)
        repoStatus.runStatus()
      }
      case "status-branch" => {
        val branchName = config.branchName.getOrElse("default")
        val repoStatus = new RepoStatus(config)
        repoStatus.runBranchStatus(branchName)
      }
      case "status" => {
        System.err.println("To be implemented.")
      }
      case _        => showHelpText
    }
  }


  private def runSuiteDiff(rev1: Option[String], rev2: Option[String]): Unit = {
    // Select whether to run sleek, hip or both
    val resultPairs: (Commit, Commit) => DiffableResults = if (config.isRunAll) {
      allResultPairs
    } else if (config.isRunSleek) {
      sleekResultPairs
    } else if (config.isRunHip) {
      hipResultPairs
    } else {
      throw new IllegalStateException
    }

    // Dispatch, depending on which revisions received as args
    (rev1, rev2) match {
      case (Some(r1), Some(r2)) => {
        println(s"Diff on $r1 -> $r2")

        // Using Repo.identify lets the args be shorter than 12 chars. :-)
        val revision1 = repo.identify(rev1)
        val revision2 = repo.identify(rev2)

        diffSuiteResults(revision1, revision2, resultPairs)
      }
      case (Some(r1), None) => {
        println(s"Diff on $r1 -> 'head'")

        val revision1 = repo.identify(rev1)
        val revision2 = repo.identify()

        diffSuiteResults(revision1, revision2, resultPairs)
      }
      case (None, _) => {
        // Since no rev was given, run on ...
        val revision = repo.identify()

        if (revision.isDirty) {
          // "Did working dir break anything?"
          println(s"Diff on 'head^' -> 'head+' (dirty)")

          val parentRevs = repo.parents(revision)

          parentRevs foreach { parentRevision =>
            diffSuiteResults(parentRevision, revision, resultPairs)
          }
        } else {
          println(s"Diff on 'head^' -> 'head'")

          val curRev = repo.identify()
          val parentRevs = repo.parents(curRev)

          parentRevs foreach { rev =>
            diffSuiteResults(rev, curRev, resultPairs)
          }
        }
      }
    }
  }


  private def runBisect(): Unit = {
    // TODO Could have this logic in the AppConfig itself?
    val bisectRevs = config.revs
    if (bisectRevs.length != 2) {
      throw new IllegalArgumentException("bisect should be run with 2 commits")
    }
    val initWorkingCommit = repo.identify(Some(bisectRevs(0)))
    val initFailingCommit = repo.identify(Some(bisectRevs(1)))
    val testCmd = config.bisectCmd.getOrElse(throw new IllegalArgumentException("bisect needs to be run with a command"))
    val testFile = config.bisectCmd.getOrElse(throw new IllegalArgumentException("bisect needs to be run with a file"))
    val testArgs = config.bisectArgs.mkString(" ")

    val bisectTestable = new TestCaseBuilder(Paths.get(testCmd), Paths.get(testFile), testArgs, "???")

    // assumptions/requirements:
    // * revs have linear relationship with each other
    //   - commitsInRange probably assumes same branch,
    //     which may be too strong an assumption
    // * newer rev is failing, older rev is passing

    // get proper expectedOutput for the testable
    val results = new ResultsArchive(config.resultsDir, config.buildFailuresFile)
    val workingTCR = results.resultFor(initWorkingCommit.revHash)(bisectTestable) getOrElse { 
      // probably should be a bit more robust about this
      throw new IllegalArgumentException(s"Expected to find result for $initWorkingCommit")
    }

    val bisectTC = recoverTestableFromTCR(workingTCR)

    println("running bisect...")

    bisect(initWorkingCommit, initFailingCommit, bisectTC)
  }

  private[app] def recoverTestableFromTCR(tcr: TestCaseResult): Testable = {
    // n.b. cannot recover `expectedOutput` from tcr directly.

    val bisectTestable = new TestCaseBuilder(tcr.command, tcr.filename, tcr.arguments, "???")
    val tcrExp = tcr.expected

    def recoverHipExpectedOuput(exp: List[(String, String)]): String = {
      exp map { case (k, v) => k + ": " + v } mkString(", ")
    }

    def recoverSleekExpectedOuput(exp: List[(String, String)]): String = {
      exp map { case (k, v) => v } mkString(", ")
    }

    // recover*ExpectedOutput above is awkward/magic, may be *TestCase could overload exp.
    // to allow for more appropriate types...?
    val expectedOutp =
      if (tcr.command.endsWith("hip"))
        recoverHipExpectedOuput(tcrExp)
      else if (tcr.command.endsWith("sleek"))
        recoverSleekExpectedOuput(tcrExp)
      else
        throw new UnsupportedOperationException(s"Expected command ${tcr.command} to be `sleek` or `hip`.")

    println(s"Recovered Expected output:" + expectedOutp)

    bisectTestable copy (expectedOutput = expectedOutp)
  }

  private def showHelpText(): Unit = {
    println(AppConfig.CommandLineOptionsParser.usage)
  }
}
