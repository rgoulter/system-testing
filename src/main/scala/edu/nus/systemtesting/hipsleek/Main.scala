package edu.nus.systemtesting.hipsleek

import java.nio.file.{ Files, Paths }
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.GlobalReporter
import GlobalReporter.reporter
import java.nio.file.Path
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.serialisation.TestSuiteResultJson
import edu.nus.systemtesting.FileSystemUtilities
import org.joda.time.format.ISODateTimeFormat
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.testsuite.TestSuiteResult
import scala.io.Source
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.testsuite.TestSuiteComparison

class Results(val resultsDir: String = "results") {
  /**
   * Map of `(name, rev) => File`.
   * `rev` expected to be 'short'. (12 chars).
   */
  private lazy val resultFiles = {
    val filesInDir = Paths.get(resultsDir).toFile().listFiles()

    // cf. filenameForSuiteResult
    val ResultNameRegex = "(.*)-([0-9a-f]+)-(\\d+)-(\\d+)\\.json".r
    val resultTuples = filesInDir.map({ file =>
      file.getName() match {
        case ResultNameRegex(name, rev, date, time) =>
          Some((name, rev, date + time, file))
        case _ => None
      }
    }).flatten

    // Build map of (name, rev) => [(datetimeStr, file)]
    val resultsMap = resultTuples.map({case (n,r,_,_) =>
      (n,r)
    }).toSet[(String, String)].map({ case (n,r) =>
      val dtFilePairs = resultTuples.filter({ case (name, rev, _, _) =>
        name == n && rev == r
      }).map({ case (_, _, dt, f) => (dt, f) })

      (n,r) -> dtFilePairs
    }).toMap

    // Take the latest result
    // from (name,rev) => (datetimeStr, file) map
    resultsMap.map({ case (nameRev, ls) =>
      val (latestDateTime, file) = ls.max

      nameRev -> file
    })
  }

  def resultsFor(name: String, rev: String): Option[TestSuiteResult] = {
    resultFiles.get((name, rev)).map({ file =>
      val src = Source.fromFile(file)
      val content = src.mkString
      src.close()

      TestSuiteResultJson.load(content)
    }).flatten
  }

  /** In format of `$name-$revision-$datetime.json` */
  private def filenameForSuiteResult(suiteResult: TestSuiteResult, name: String): String = {
    import suiteResult.{ repoRevision, datetime }
    val datetimeStr = datetime.toString("yyyyMMdd-HHmmss")

    s"$name-${repoRevision}-${datetimeStr}.json"
  }

  /**
   * Note for `name` that [[TestSuiteResult]] isn't necessarily from one command
   * (`hip` or `sleek`). With the current design, convenient to consider results
   * as 'hip results' or 'sleek results', though.
   *
   * @param name e.g. `"hip"`, `"sleek"`
   */
  def saveTestSuiteResult(suiteResult: TestSuiteResult, name: String): Unit = {
    import suiteResult.{ repoRevision, datetime }

    FileSystemUtilities.checkOutputDirectory(resultsDir)
    val filename = filenameForSuiteResult(suiteResult, name)
    val dump = TestSuiteResultJson.dump(suiteResult)

    val path = Paths.get(resultsDir, filename)
    reporter.log(s"\nSaving results to $path\n")
    FileSystemUtilities.printToFile(path.toFile())(_.print(dump))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val appCfg = AppConfig.load()

    // Override options from loaded config with CLAs
    AppConfig.CommandLineOptionsParser.parse(args, appCfg) match {
      case Some(config) => {
        import config.{ command, repoDir, rev }

        // do stuff
        command match {
          case "sleek" => runSleekTests(repoDir, rev)
          case "hip" => runHipTests(repoDir, rev)
          case "all" => runAllTests(repoDir, rev)
          case "svcomp" => runSVCompTests
          case "diff" => runSuiteDiff(repoDir, config.rev1, config.rev2)
          case _ => showHelpText
        }
      }

      case None => ()
    }
  }

  private def runAllTests(repoDir: Path, rev: Option[String]): Unit = {
    // XXX: The logic is somewhat involved at the moment to check both results
    //      present, so, better to just delegate
    runSleekTests(repoDir, rev)
    runHipTests(repoDir, rev)
  }

  private def runSleekTests(repoDir: Path, rev: Option[String]): Option[TestSuiteResult] = {
    // XXX: Assume repoDir is repo
    val repo = new Repository(repoDir)
    val revision = rev.getOrElse(repo.identify())

    (new Results).resultsFor("sleek", revision) match {
      case Some(testSuiteResult) if !repo.isDirty() => {
        reporter.log(s"sleek testsuite results found for $revision.")

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
    // XXX: Assume repoDir is repo
    val repo = new Repository(repoDir)
    val revision = rev.getOrElse(repo.identify())

    (new Results).resultsFor("hip", revision) match {
      case Some(testSuiteResult) if !repo.isDirty() => {
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

  private def runTestsWith(repoDir: Path, rev: Option[String])
                          (f: (Path, String) => Option[TestSuiteResult]):
      Option[TestSuiteResult] = {
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
  private def runTestsWithRepo(repoDir: Path, rev: Option[String])
                              (f: (Path, String) => Option[TestSuiteResult]):
      Option[TestSuiteResult] = {
    // Prepare the repo, if necessary
    reporter.log("Preparing repo...")

    val repo = new Repository(repoDir)
    val revision = rev.getOrElse(repo.identify())

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
  private def runTestsWithFolder(projectDir: Path, rev: Option[String])
                                (f: (Path, String) => Option[TestSuiteResult]):
      Option[TestSuiteResult] = {
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

    (new Results).saveTestSuiteResult(res, "sleek")

    Some(res)
  }

  /** Assumes that the project dir has been prepared successfully */
  private def runPreparedHipTests(projectDir: Path, revision: String): Option[TestSuiteResult] = {
    reporter.header("Running Hip Tests")

    val significantTime = 1 // CONFIG ME
    val testCaseTimeout = 300
    val suite = new HipTestSuiteUsage(projectDir, significantTime, testCaseTimeout, revision)

    val res = suite.run()

    (new Results).saveTestSuiteResult(res, "hip")

    Some(res)
  }

  private def runSVCompTests(): Unit = {
    reporter.header("Running SVComp Tests")
    SVCompTestSuiteUsage.run()
  }

  private def runSuiteDiff(repoDir: Path, rev1: Option[String], rev2: Option[String]): Unit = {
    /*
     * The hip/sleek disjunction in saving/loading results is a bit annoying for now,
     * makes "load [or-else-run] to get results" hard, makes "diff" tedious.
     * Surely there's some elegant way to achieve this?
     *
     * XXX: For now, diff only on Sleek tests (since Hip takes ~ 2 + 9 minutes to run).
     */

    val repo = new Repository(repoDir)

    (rev1, rev2) match {
      case (Some(r1), Some(r2)) => {
        println(s"Diff on $r1 -> $r2")

        diffSuiteResults(repoDir, r1, r2)
      }
      case (Some(r1), None) => {
        println(s"Diff on $r1 -> 'head'")
        val r2 = repo.identify()

        diffSuiteResults(repoDir, r1, r2)
      }
      case (None, _) => {
        println(s"Diff on 'head^' -> 'head'")
        // TODO
      }
    }
  }

  private def diffSuiteResults(repoDir: Path, rev1: String, rev2: String): Unit = {
    // n.b. `run` is misnomer here; will load if results available
    val maybeOldRes = runSleekTests(repoDir, Some(rev1))
    val maybeCurRes = runSleekTests(repoDir, Some(rev2))

    (maybeOldRes, maybeCurRes) match {
      case (Some(oldTSRes), Some(curTSRes)) => {
        val diff = TestSuiteComparison(oldTSRes, curTSRes)

        diff.displayResult()
      }

      case _ => {
        reporter.log(s"Results unavailable for one of $rev1 or $rev2")
      }
    }
  }

  private def showHelpText(): Unit = {
    println(AppConfig.CommandLineOptionsParser.usage)
  }
}
