package edu.nus.systemtesting.hipsleek.app

import java.nio.file.{ Files, Path, Paths }
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.serialisation.ResultsArchive
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.output.GlobalReporter.reporter
import com.typesafe.config.ConfigFactory
import edu.nus.systemtesting.FileSystemUtilities
import java.io.IOException
import com.typesafe.config.ConfigException
import edu.nus.systemtesting.output.ANSIReporter
import edu.nus.systemtesting.hipsleek.HipSleekPreparation
import edu.nus.systemtesting.hipsleek.HipTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SVCompTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SleekTestSuiteUsage
import edu.nus.systemtesting.hipsleek.TestSuiteResultAnalysis
import edu.nus.systemtesting.output.VisibilityOptions

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
  private[hipsleek] def run(): Unit = {
    import config.{ command, rev }

    val repoDir: Path = config.repoDir getOrElse {
      System.err.println(
          """Unable to find REPO_DIR. Try:
            | * Running the program in the mercurial repository, or
            |   a descendant folder of a mercurial repo.
            | * Putting a .hipsleektest.conf file with REPO_DIR=/path/to/repo line
            |   in the current directory, or in some ancestor folder of the CWD.
            | * Compiling this program with an application.conf with REPO_DIR=/path/to/repo line""".stripMargin)
      // 'Fatal' error, quit.
      System.exit(1)
      throw new IllegalStateException
    }

    if (!(repoDir resolve ".hg").toFile().exists()) {
      System.err.println(s"ERROR! Not a Mercurial repository! REPODIR=$repoDir")
      System.exit(1)
    }

    command match {
      case "sleek"  => runSleekTests(repoDir, rev)
      case "hip"    => runHipTests(repoDir, rev)
      case "all"    => runAllTests(repoDir, rev)
      case "svcomp" => runSVCompTests
      case "diff"   => runSuiteDiff(repoDir, config.rev1, config.rev2)
      case _        => showHelpText
    }
  }

  private def runAllTests(repoDir: Path, rev: Option[String]):
      Option[(TestSuiteResult, TestSuiteResult)] = {
    (for {
      sleekRes <- results(repoDir, rev, "sleek")
      hipRes <- results(repoDir, rev, "hip")
    } yield (sleekRes, hipRes)) match {
      case rtn@Some((sleekTSRes, hipTSRes)) => {

        val revision = sleekTSRes.repoRevision

        reporter.log(s"Found sleek testsuite results for $revision.")
        sleekTSRes.displayResult(config.significantTimeThreshold)
        TestSuiteResultAnalysis printTallyOfInvalidTests sleekTSRes
        hipTSRes.displayResult(config.significantTimeThreshold)
        TestSuiteResultAnalysis printTallyOfInvalidTests hipTSRes

        rtn
      }

      // No results found, so, must run the prog. to get results
      case None => {
        reporter.log("sleek,hip testsuite results not found, running test suites...")
        runTestsWith(repoDir, rev, "examples/working") { case (binDir, corpusDir, revision) =>
          for {
            sleek <- runPreparedSleekTests(binDir, corpusDir resolve "sleek", revision)
            hip   <- runPreparedHipTests(binDir, corpusDir resolve "hip", revision)
          } yield (sleek, hip)
        }
      }
    }
  }

  private def runSleekTests(repoDir: Path, rev: Option[String]): Option[TestSuiteResult] = {
    results(repoDir, rev, "sleek") match {
      case Some(testSuiteResult) => {
        val revision = testSuiteResult.repoRevision

        reporter.log(s"Found sleek testsuite results for $revision.")
        testSuiteResult.displayResult(config.significantTimeThreshold)
        TestSuiteResultAnalysis printTallyOfInvalidTests testSuiteResult

        Some(testSuiteResult)
      }

      // No results found, so, must run the prog. to get results
      case None => {
        reporter.log("sleek testsuite results not found, running test suite...")
        runTestsWith(repoDir, rev, "examples/working/sleek")(runPreparedSleekTests)
      }
    }
  }

  private def runHipTests(repoDir: Path, rev: Option[String]): Option[TestSuiteResult] = {
    results(repoDir, rev, "hip") match {
      case Some(testSuiteResult) => {
        val revision = testSuiteResult.repoRevision

        reporter.log(s"Found hip testsuite results for $revision.")
        testSuiteResult.displayResult(config.significantTimeThreshold)
        TestSuiteResultAnalysis printTallyOfInvalidTests testSuiteResult

        Some(testSuiteResult)
      }

      // No results found, so, must run the prog. to get results
      case None => {
        reporter.log("hip testsuite results not found, running test suite...")
        runTestsWith(repoDir, rev, "examples/working/hip")(runPreparedHipTests)
      }
    }
  }

  private def results(repoDir: Path, rev: Option[String], name: String): Option[TestSuiteResult] = {
    val repo = new Repository(repoDir)
    val revision = repo.identify(rev)

    if (!repo.isDirty()) {
      // TODO: Also should check if the results we get is 'the same' as
      //       the tests we want to run.
      (new ResultsArchive).resultsFor(name, revision)
    } else {
      None
    }
  }

  private def runTestsWith[T](repoDir: Path, rev: Option[String], examplesDir: String)
                             (f: (Path, Path, String) => Option[T]):
      Option[T] = {
    runTestsWithRepo(repoDir, rev, examplesDir)(f)
  }

  /**
   * `repoDir` is assumed to be a repository. This method creates an archive
   * of the repo in a tmp directory if the revision is specified, or
   * if the repository is 'clean'.
   *
   * If the repository is dirty, the repository's working directory is used
   * to run, and it is assumed that this folder can be used to make, and
   * run the tests in.
   */
  private def runTestsWithRepo[T](repoDir: Path, rev: Option[String], examplesDir: String)
                                 (f: (Path, Path, String) => Option[T]):
      Option[T] = {
    // Prepare the repo, if necessary
    reporter.log("Preparing repo...")

    val repo = new Repository(repoDir)
    val revision = repo.identify(rev)

    val isDirty = rev match {
      case Some(s) => false
      // If no rev given, use Working Directory of repo.
      case None => repo.isDirty()
    }

    withTmpDir("edunussystest") { tmpDir =>
      val projectDir = if (isDirty) {
        // i.e. LIVE, "in place"
        repoDir
      } else {
        val tmp = tmpDir.toAbsolutePath()

        // create archive of repo in tmp
        repo.archive(tmp, rev)

        tmp
      }

      val prep = new HipSleekPreparation(projectDir)
      val (prepWorked, prepRemarks) = prep.prepare()

      prepRemarks.foreach(reporter.log)
      reporter.println()

      // Run the tests
      if (prepWorked) {
        // TODO: Copy to cache..

        val binDir = projectDir
        val corpusDir = projectDir resolve examplesDir

        f(binDir, corpusDir, revision)
      } else {
        None
      }
    }
  }

  /**
   * @param f synchronous function; tmpdir is removed after f finishes
   */
  private def withTmpDir[T](name: String = "edunussystest",
                            removeAfterUse: Boolean = true)
                           (f: Path => T): T = {
    val tmpDir = Files createTempDirectory "edunussystest"

    val rtn = f(tmpDir)

    // Finished running the tests, clean up.
    try {
      if (removeAfterUse) {
        reporter.log("Deleting " + tmpDir)
        FileSystemUtilities rmdir tmpDir
      }
    } catch {
      case ioEx: IOException => {
        System.err.println(s"Unable to delete dir $tmpDir")
        ioEx.printStackTrace()
      }
    }

    rtn
  }

  /** Assumes that the project dir has been prepared successfully */
  private def runPreparedSleekTests(binDir: Path, examplesDir: Path, revision: String): Option[TestSuiteResult] = {
    reporter.header("Running Sleek Tests")

    val significantTime = config.significantTimeThreshold
    val testCaseTimeout = config.timeout
    val suite = new SleekTestSuiteUsage(binDir,
                                        significantTime,
                                        testCaseTimeout,
                                        revision,
                                        examplesDir = examplesDir)

    val res = suite.run()

    (new ResultsArchive).saveTestSuiteResult(res, "sleek")

    Some(res)
  }

  /** Assumes that the project dir has been prepared successfully */
  private def runPreparedHipTests(binDir: Path, examplesDir: Path, revision: String): Option[TestSuiteResult] = {
    reporter.header("Running Hip Tests")

    val significantTime = config.significantTimeThreshold
    val testCaseTimeout = config.timeout
    val suite = new HipTestSuiteUsage(binDir,
                                      significantTime,
                                      testCaseTimeout,
                                      revision,
                                      examplesDir = examplesDir)

    val res = suite.run()

    (new ResultsArchive).saveTestSuiteResult(res, "hip")

    Some(res)
  }

  private def runSVCompTests(): Unit = {
    reporter.header("Running SVComp Tests")
    SVCompTestSuiteUsage.run()
  }

  private def runSuiteDiff(repoDir: Path, rev1: Option[String], rev2: Option[String]): Unit = {
    val repo = new Repository(repoDir)

    // Select whether to run sleek, hip or both
    val resultPairs: (Path, String, Option[String]) => DiffableResults = if (config.isRunAll) {
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

        diffSuiteResults(repoDir, r1, rev2, resultPairs)
      }
      case (Some(r1), None) => {
        println(s"Diff on $r1 -> 'head'")
        val r2 = repo.identify()

        diffSuiteResults(repoDir, r1, rev2, resultPairs)
      }
      case (None, _) => {
        // Since no rev was given, run on ...
        if (repo.isDirty()) {
          // "Did working dir break anything?"
          println(s"Diff on 'head^' -> 'head+' (dirty)")

          val parentRevs = repo.parents(None)

          parentRevs foreach { rev =>
            diffSuiteResults(repoDir, rev, None, resultPairs)
          }
        } else {
          println(s"Diff on 'head^' -> 'head'")

          val curRev = repo.identify()
          val parentRevs = repo.parents(Some(curRev))

          parentRevs foreach { rev =>
            diffSuiteResults(repoDir, rev, Some(curRev), resultPairs)
          }
        }
      }
    }
  }

  /** Used for `diffSuiteResults`, to save typing / screen space. */
  type DiffableResults = List[(String, TestSuiteResult, TestSuiteResult)]

  /** For use with `diffSuiteResults`, for running just sleek results. */
  private def sleekResultPairs(repoDir: Path, rev1: String, rev2: Option[String]):
      DiffableResults = {
    (for {
      oldRes <- runSleekTests(repoDir, Some(rev1))
      curRes <- runSleekTests(repoDir, rev2)
    } yield ("sleek", oldRes, curRes)).toList
  }

  /** For use with `diffSuiteResults`, for running just hip results. */
  private def hipResultPairs(repoDir: Path, rev1: String, rev2: Option[String]):
      DiffableResults = {
    (for {
      oldRes <- runHipTests(repoDir, Some(rev1))
      curRes <- runHipTests(repoDir, rev2)
    } yield ("hip", oldRes, curRes)).toList
  }

  /**
   * For use with `diffSuiteResults`, for running both sleek, hip results.
   *
   * The way it is implemented, the output of `diffSuiteResults` won't combine
   * the diff results together, so sleek diff will be followed by hip diff.
   */
  private def allResultPairs(repoDir: Path, rev1: String, rev2: Option[String]):
      DiffableResults = {
    (for {
      (oldSleekRes, oldHipRes) <- runAllTests(repoDir, Some(rev1))
      (curSleekRes, curHipRes) <- runAllTests(repoDir, rev2)
    } yield List(("sleek", oldSleekRes, curSleekRes),
                 ("hip", oldHipRes, curHipRes))).toList.flatten
  }

  private def diffSuiteResults(repoDir: Path,
                               rev1: String,
                               rev2: Option[String],
                               resultsFor: (Path, String, Option[String]) => DiffableResults): Unit = {
    val diffable = resultsFor(repoDir, rev1, rev2)

    if (!diffable.isEmpty) {
      diffable foreach { case (name, oldTSRes, curTSRes) =>
        val diff = TestSuiteComparison(oldTSRes, curTSRes)

        diff.display(name)
      }
    } else {
      reporter.log(s"Results unavailable for one of $rev1 or $rev2")
    }
  }

  private def showHelpText(): Unit = {
    println(AppConfig.CommandLineOptionsParser.usage)
  }
}
