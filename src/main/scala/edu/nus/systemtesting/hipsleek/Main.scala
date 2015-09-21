package edu.nus.systemtesting.hipsleek

import java.nio.file.{ Files, Path, Paths }
import scala.io.Source
import org.joda.time.format.ISODateTimeFormat

import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.serialisation.ResultsArchive
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.testsuite.TestSuiteComparison

import GlobalReporter.reporter

object Main {
  def main(args: Array[String]): Unit = {
    val appCfg = AppConfig.load()

    // Override options from loaded config with CLAs,
    // run with configuration.
    AppConfig.CommandLineOptionsParser.parse(args, appCfg) foreach { config =>
      val configuredMain = new ConfiguredMain(config)
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
    import config.{ command, repoDir, rev }

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
        sleekTSRes.displayResult() // CONFIG ME
        hipTSRes.displayResult() // CONFIG ME

        rtn
      }

      // No results found, so, must run the prog. to get results
      case None => {
        reporter.log("sleek,hip testsuite results not found, running test suites...")
        runTestsWith(repoDir, rev) { case (projDir, revision) =>
          for {
            sleek <- runPreparedSleekTests(projDir, revision)
            hip   <- runPreparedHipTests(projDir, revision)
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
        testSuiteResult.displayResult() // CONFIG ME

        Some(testSuiteResult)
      }

      // No results found, so, must run the prog. to get results
      case None => {
        reporter.log("sleek testsuite results not found, running test suite...")
        runTestsWith(repoDir, rev)(runPreparedSleekTests)
      }
    }
  }

  private def runHipTests(repoDir: Path, rev: Option[String]): Option[TestSuiteResult] = {
    results(repoDir, rev, "hip") match {
      case Some(testSuiteResult) => {
        val revision = testSuiteResult.repoRevision

        reporter.log(s"Found hip testsuite results for $revision.")
        testSuiteResult.displayResult() // CONFIG ME

        Some(testSuiteResult)
      }

      // No results found, so, must run the prog. to get results
      case None => {
        reporter.log("hip testsuite results not found, running test suite...")
        runTestsWith(repoDir, rev)(runPreparedHipTests)
      }
    }
  }

  private def results(repoDir: Path, rev: Option[String], name: String): Option[TestSuiteResult] = {
    val isRepo = (repoDir resolve ".hg").toFile().exists()

    if (isRepo) {
      val repo = new Repository(repoDir)
      val revision = repo.identify(rev)

      if (!repo.isDirty()) {
        // TODO: Also should check if the results we get is 'the same' as
        //       the tests we want to run.
        (new ResultsArchive).resultsFor("hip", revision)
      } else {
        None
      }
    } else {
      None
    }
  }

  private def runTestsWith[T](repoDir: Path, rev: Option[String])
                             (f: (Path, String) => Option[T]):
      Option[T] = {
    val isRepo = (repoDir resolve ".hg").toFile().exists()

    if (isRepo) {
      runTestsWithRepo(repoDir, rev)(f)
    } else {
      runTestsWithFolder(repoDir, rev)(f)
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
  private def runTestsWithRepo[T](repoDir: Path, rev: Option[String])
                                (f: (Path, String) => Option[T]):
      Option[T] = {
    // Prepare the repo, if necessary
    reporter.log("Preparing repo...")

    val repo = new Repository(repoDir)
    val revision = repo.identify(rev)

    val isDirty = rev match {
      // Assumes that given rev isn't a "dirty" one;
      // e.g. a call to "hg update <rev>" would make sense
      case Some(s) => false
      // If no rev given, use Working Directory of repo.
      case None => repo.isDirty()
    }

    val tmpDir = Files.createTempDirectory("edunussystest")

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
    val rtn = if (prepWorked) f(projectDir, revision) else None

    // Finished running the tests, clean up.
    tmpDir.toFile().delete()

    rtn
  }

  /**
   * `projectDir` not assumed to be repository.
   * It *is* assumed that `projectDir` will be used for making, running the
   * executables/tests.
   */
  private def runTestsWithFolder[T](projectDir: Path, rev: Option[String])
                                   (f: (Path, String) => Option[T]):
      Option[T] = {
    // i.e. LIVE, "in place"
    val revision = rev.getOrElse("unknown")

    // Prepare the repo, if necessary
    reporter.log("Preparing folder...")

    val prep = new HipSleekPreparation(projectDir)
    val (prepWorked, prepRemarks) = prep.prepare()

    prepRemarks.foreach(reporter.log)
    reporter.println()

    // Run the tests
    if (prepWorked)
      f(projectDir, revision)
    else
      None
  }

  /** Assumes that the project dir has been prepared successfully */
  private def runPreparedSleekTests(projectDir: Path, revision: String): Option[TestSuiteResult] = {
    reporter.header("Running Sleek Tests")

    val significantTime = 1 // CONFIG ME
    val testCaseTimeout = 300
    val suite = new SleekTestSuiteUsage(projectDir, significantTime, testCaseTimeout, revision)

    val res = suite.run()

    (new ResultsArchive).saveTestSuiteResult(res, "sleek")

    Some(res)
  }

  /** Assumes that the project dir has been prepared successfully */
  private def runPreparedHipTests(projectDir: Path, revision: String): Option[TestSuiteResult] = {
    reporter.header("Running Hip Tests")

    val significantTime = 1 // CONFIG ME
    val testCaseTimeout = 300
    val suite = new HipTestSuiteUsage(projectDir, significantTime, testCaseTimeout, revision)

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


    (rev1, rev2) match {
      case (Some(r1), Some(r2)) => {
        println(s"Diff on $r1 -> $r2")

        diffSuiteResults(repoDir, r1, r2, sleekResultPairs)
      }
      case (Some(r1), None) => {
        println(s"Diff on $r1 -> 'head'")
        val r2 = repo.identify()

        diffSuiteResults(repoDir, r1, r2, sleekResultPairs)
      }
      case (None, _) => {
        println(s"Diff on 'head^' -> 'head'")
        // TODO
      }
    }
  }

  /** Used for `diffSuiteResults`, to save typing / screen space. */
  type DiffableResults = List[(String, TestSuiteResult, TestSuiteResult)]

  /** For use with `diffSuiteResults`, for running just sleek results. */
  private def sleekResultPairs(repoDir: Path, rev1: String, rev2: String):
      DiffableResults = {
    (for {
      oldRes <- runSleekTests(repoDir, Some(rev1))
      curRes <- runSleekTests(repoDir, Some(rev2))
    } yield ("sleek", oldRes, curRes)).toList
  }

  /** For use with `diffSuiteResults`, for running just hip results. */
  private def hipResultPairs(repoDir: Path, rev1: String, rev2: String):
      DiffableResults = {
    (for {
      oldRes <- runHipTests(repoDir, Some(rev1))
      curRes <- runHipTests(repoDir, Some(rev2))
    } yield ("hip", oldRes, curRes)).toList
  }

  /**
   * For use with `diffSuiteResults`, for running both sleek, hip results.
   *
   * The way it is implemented, the output of `diffSuiteResults` won't combine
   * the diff results together, so sleek diff will be followed by hip diff.
   */
  private def allResultPairs(repoDir: Path, rev1: String, rev2: String):
      DiffableResults = {
    (for {
      (oldSleekRes, oldHipRes) <- runAllTests(repoDir, Some(rev1))
      (curSleekRes, curHipRes) <- runAllTests(repoDir, Some(rev2))
    } yield List(("sleek", oldSleekRes, curSleekRes),
                 ("hip", oldHipRes, curHipRes))).toList.flatten
  }

  private def diffSuiteResults(repoDir: Path,
                               rev1: String,
                               rev2: String,
                               resultsFor: (Path, String, String) => DiffableResults): Unit = {
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
