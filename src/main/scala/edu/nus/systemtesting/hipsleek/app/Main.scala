package edu.nus.systemtesting.hipsleek.app

import java.io.IOException
import java.nio.file.{ Files, Path, Paths }
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.Duration
import edu.nus.systemtesting.BinCache
import edu.nus.systemtesting.FileSystemUtilities
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseConfiguration
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.hipsleek.HipSleekPreparation
import edu.nus.systemtesting.hipsleek.HipTestCase
import edu.nus.systemtesting.hipsleek.HipTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SleekTestCase
import edu.nus.systemtesting.hipsleek.SleekTestSuiteUsage
import edu.nus.systemtesting.hipsleek.TestSuiteResultAnalysis
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.output.ANSIReporter
import edu.nus.systemtesting.output.VisibilityOptions
import edu.nus.systemtesting.serialisation.ResultsArchive
import edu.nus.systemtesting.testsuite.TestSuite
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException
import edu.nus.systemtesting.TestCaseConfiguration

class UnableToBuildException(repoDir: Path,
                             rev: Option[String])
  extends RuntimeException(s"Cannot build for revision ${rev getOrElse ""} in dir $repoDir")

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
  val binCache = new BinCache() // config me

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
      case "diff"   => runSuiteDiff(repoDir, config.rev1, config.rev2)
      case _        => showHelpText
    }
  }

  type RunPreparedTests = (Path, Path, String) => TestSuiteResult

  private def runAllTests(repoDir: Path, rev: Option[String]): Option[List[TestSuiteResult]] =
    Some(List(altRunTests(HipTestCase.constructTestCase,
                          "hip",
                          HipTestSuiteUsage.allTestable)(repoDir, rev),
              altRunTests(SleekTestCase.constructTestCase,
                          "sleek",
                          SleekTestSuiteUsage.allTestable)(repoDir, rev)))

  private def runHipTests(repoDir: Path, rev: Option[String]): Option[List[TestSuiteResult]] =
    Some(List(altRunTests(HipTestCase.constructTestCase,
                          "hip",
                          HipTestSuiteUsage.allTestable)(repoDir, rev)))

  private def runSleekTests(repoDir: Path, rev: Option[String]): Option[List[TestSuiteResult]] =
    Some(List(altRunTests(SleekTestCase.constructTestCase,
                          "sleek",
                          SleekTestSuiteUsage.allTestable)(repoDir, rev)))

  // construct e.g. HipTestCase.constructTestCase
  private def altRunTests(construct: (PreparedSystem, Testable, TestCaseConfiguration) => TestCase,
                          suiteName: String,
                          allTestable: List[Testable])
                         (repoDir: Path,
                          rev: Option[String]): TestSuiteResult = {
    (runTestsWith(repoDir, rev, "examples/working/" + suiteName) { case (binDir, corpusDir, repoRevision) =>
      // Ideally, preparedSys would itself do the building of repo.
      // i.e. building the repo would be delayed until necessary.
      // At the moment, though, since any system loading tests will *have* the
      // tests, this is not going to slow things down.
      lazy val preparedSys = PreparedSystem(binDir, corpusDir)

      val resultsFor = runTestCaseForRevision(repoRevision, preparedSys)(construct)

      val suite = suiteFor(allTestable, repoRevision)

      // runPrepared just gets TSuite from *TSUsage, then calls runAll w/ archive, rtn result
      val testSuiteResult = suite.runAllTests(resultsFor)

      // displayResult blocks until all results computed
      testSuiteResult.displayResult(config.significantTimeThreshold)
      TestSuiteResultAnalysis printTallyOfInvalidTests testSuiteResult

      testSuiteResult
    }) getOrElse {
      // Failed to build is the only reason runTestsWith will return None
      throw new UnableToBuildException(repoDir, rev)
    }
  }

  private def runTestsWith[T](repoDir: Path, rev: Option[String], examplesDir: String)
                             (f: (Path, Path, String) => T):
      Option[T] = {
    // check if bin cache has the binaries already
    val repo = new Repository(repoDir)
    val revision = repo.identify(rev)

    binCache.binFor(Paths.get("hip"), revision) match {
      case Some(p) => {
        val binDir = p getParent()
        Some(runTestsWithCached(repoDir, binDir, rev, examplesDir)(f))
      }

      case None =>
        runTestsWithRepo(repoDir, rev, examplesDir)(f)
    }
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
                                 (f: (Path, Path, String) => T):
      Option[T] = {
    // Prepare the repo
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
        val binDir = projectDir
        val corpusDir = projectDir resolve examplesDir

        // Copy to cache..
        // n.b. revision from repo.identify. (a type might help ensure it's 12 char long..)
        binCache.cache(binDir, Paths.get("sleek"), revision)
        binCache.cache(binDir, Paths.get("hip"), revision)
        // apparently prelude.ss needs to be in, or hip will break.
        binCache.cache(binDir, Paths.get("prelude.ss"), revision)

        Some(f(binDir, corpusDir, revision))
      } else {
        None
      }
    }
  }

  private def runTestsWithCached[T](repoDir: Path, binDir: Path, rev: Option[String], examplesDir: String)
                                   (f: (Path, Path, String) => T): T = {
    // don't know whether it's hip/sleek we want, but we make/cache both, so.
    require((binDir resolve "sleek").toFile().exists())
    require((binDir resolve "hip").toFile().exists())

    reporter.log("Using cached binaries...")

    val repo = new Repository(repoDir)
    val revision = repo.identify(rev)

    val isDirty = rev match {
      case Some(s) => false
      // If no rev given, use Working Directory of repo.
      case None => repo.isDirty()
    }


    // Need to have corpusDir; easiest to get from archive.
    // May be nice if could just "hg cat" (or so) over a bunch of files,
    //  might save time.
    withTmpDir("edunussystest") { tmpDir =>
      val projectDir = if (isDirty) {
        // i.e. LIVE, "in place",
        // esp. in case user makes use of example they modified/added
        repoDir
      } else {
        val tmp = tmpDir.toAbsolutePath()

        // Folders used by e.g. SleekTestSuiteUsage, HipTestSuiteUsage
        // TODO Hardcoded for now, due to architecture.
        val foldersUsed =  List(
          "examples/working/sleek",
          "examples/working/hip",
          "examples/working/hip_baga",
          "examples/working/infer",
          "examples/working/tree_shares",
          "examples/modular_examples"
        )

        // create archive of repo in tmp
        repo.archive(tmp, rev, foldersUsed)

        tmp
      }

      val corpusDir = projectDir resolve examplesDir

      f(binDir, corpusDir, revision)
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

  // should be able to replace runWith, callback nature with this...
  private def runTestCaseForRevision(repoRevision: String, preparedSys: => PreparedSystem)
                                    (implicit construct: (PreparedSystem, Testable, TestCaseConfiguration) => TestCase):
      (Testable => TestCaseResult) = {
    val resultsArch = new ResultsArchive()

    { tc: Testable =>
      resultsArch.resultFor(repoRevision)(tc) match {
        case Some(tcr) => tcr

        case None => {
          val conf = TestCaseConfiguration(timeout = config.timeout)
          val tcase = construct(preparedSys, tc, conf)

          // Run the TestCase, save results
          val tcr = tcase.generateOutput

          // Didn't have results, save them.
          try {
            resultsArch.saveTestCaseResult(repoRevision, tcr)
          } catch {
            case e: Throwable => {
              e.printStackTrace()
            }
          }

          tcr
        }
      }
    }
  }

  private def suiteFor(allTests: List[Testable], repoRevision: String): TestSuite = {
    new TestSuite(allTests, repoRevision, config.significantTimeThreshold)
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

  // The magic used below is a bit annoying,
  // but to work around it, 
  // while runSleekTests, runHipTests, runAllTests only return 1-2 things,
  // is over-engineering it..

  /** For use with `diffSuiteResults`, for running just sleek results. */
  private def sleekResultPairs(repoDir: Path, rev1: String, rev2: Option[String]):
      DiffableResults = {
    (for {
      oldRes <- runSleekTests(repoDir, Some(rev1))
      curRes <- runSleekTests(repoDir, rev2)
    } yield ("sleek", oldRes(0), curRes(0))).toList
  }

  /** For use with `diffSuiteResults`, for running just hip results. */
  private def hipResultPairs(repoDir: Path, rev1: String, rev2: Option[String]):
      DiffableResults = {
    (for {
      oldRes <- runHipTests(repoDir, Some(rev1))
      curRes <- runHipTests(repoDir, rev2)
    } yield ("hip", oldRes(0), curRes(0))).toList
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
      oldSuiteResults <- runAllTests(repoDir, Some(rev1))
      curSuiteResults <- runAllTests(repoDir, rev2)
    } yield List(("sleek", oldSuiteResults(0), curSuiteResults(0)),
                 ("hip",   oldSuiteResults(1), curSuiteResults(1)))).toList.flatten
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
