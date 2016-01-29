package edu.nus.systemtesting.hipsleek.app

import java.io.File
import java.io.PrintWriter
import java.nio.file.Path
import java.nio.file.Paths
import scala.io.Source
import scala.concurrent.Channel
import edu.nus.systemtesting.ExpectsOutput
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseConfiguration
import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.hipsleek.BuildResult
import edu.nus.systemtesting.hipsleek.HipTestCase
import edu.nus.systemtesting.hipsleek.HipTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SleekTestCase
import edu.nus.systemtesting.hipsleek.SleekTestSuiteUsage
import edu.nus.systemtesting.hipsleek.ValidateableSleekTestCase
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.ConstructTestCase

/**
 * Generates, runs a subset of some set of testables.
 *
 * Does not deal with legacy, misnamed `run-fast-tests.pl`.
 */
class RunFast(config: AppConfig) extends UsesRepository(config) {
  val validate = new Validate(config)


  def DefaultFastCacheName(name: String) = s".fasttests_$name"

  // might make sense to use SQLite db here.

  private case class FastTestRow(filename: String, args: Option[String] = None) {
    override def toString(): String =
      filename + (args map (a => " " + a) getOrElse "")
  }

  private def loadFastTests(file: File): Option[List[FastTestRow]] = {
    if (file.exists) {
      val src = Source fromFile file
      val content = src.mkString
      src.close()

      Some(content.lines.toList map { line =>
        line.split(" ", 2) match {
          case Array(filename) => FastTestRow(filename)
          case Array(filename, args) => FastTestRow(filename, Some(args))
        }
      })
    } else {
      None
    }
  }

  private def saveToFile(file: File, rows: List[FastTestRow]): Unit = {
    val out = new PrintWriter(file)

    rows foreach { row =>
      out.println(row.toString())
    }

    out.flush()
    out.close()
  }

  private def fileForSuite(suite: Suite): File =
    new File(DefaultFastCacheName(suite.toString()))



  private def args(arguments: String): Option[String] =
    if (arguments == "") None else Some(arguments)

  private def rowFromTestable(tc: Testable): FastTestRow = {
    import tc.{ fileName, arguments }

    FastTestRow(fileName.toString(), args(arguments))
  }

  private def filterTestable(all: List[Testable with ExpectsOutput],
                             loaded: List[FastTestRow]): List[Testable with ExpectsOutput] = {
    val loadedSet = loaded.toSet

    all filter { tc => loadedSet contains rowFromTestable(tc) }
  }

  private def loadFastTests(suite: Suite): Option[List[Testable with ExpectsOutput]] = {
    loadFastTests(fileForSuite(suite)) map { fastTests =>
      suite match {
        case HipOnly()   => filterTestable(HipTestSuiteUsage.allTestable, fastTests)
        case SleekOnly() => filterTestable(SleekTestSuiteUsage.allTestable, fastTests)

        // This is a little more dubious,
        // since typically Sleek+Hip are treated distinctly.
        case All() => filterTestable(SleekTestSuiteUsage.allTestable ++ HipTestSuiteUsage.allTestable, fastTests)

        case SleekValidateOnly() => {
          // construct ValidateableSleekTestCase directly
          fastTests map { fastTest =>
            TestCaseBuilder(Paths.get("sleek"), Paths.get(fastTest.filename))
          }
        }
      }
    }
  }

  private def saveFastTests(suite: Suite, data: List[Testable]): Unit = {
    saveToFile(fileForSuite(suite), data map rowFromTestable)
  }

  private def allConstructTestCase(ps: PreparedSystem, tc: Testable with ExpectsOutput, conf: TestCaseConfiguration):
      TestCase = {
    if (tc.toString endsWith "hip") {
      HipTestCase.constructTestCase(ps, tc, conf)
    } else {
      // assume if-not-hip then must be sleek
      SleekTestCase.constructTestCase(ps, tc, conf)
    }
  }

  private def constructForSuite(suite: Suite): ConstructTestCase =
    suite match {
      case HipOnly()           => HipTestCase.constructTestCase
      case SleekOnly()         => SleekTestCase.constructTestCase

      // This is more involved;
      case All()               => allConstructTestCase
      case SleekValidateOnly() => ValidateableSleekTestCase.constructTestCase
    }

  private def allTestableForSuite(suite: Suite): List[Testable with ExpectsOutput] =
    suite match {
      case HipOnly()           => HipTestSuiteUsage.allTestable
      case SleekOnly()         => SleekTestSuiteUsage.allTestable
      case All()               => SleekTestSuiteUsage.allTestable ++ HipTestSuiteUsage.allTestable
      case SleekValidateOnly() => validate.allTestable
    }

  private def generateFastTestablesForSuite(suite: Suite): List[Testable] = {
    // get the universe of testable for the suite,
    val allTestable = allTestableForSuite(suite)
    val construct = constructForSuite(suite)

    // Try to keep the timing under 2 mins for running the tests
    // TODO RunFast customise FastTestTime
    val FastTestTime = 120 * 1000


    val resArch = config.defaultResultsArchive
    val extantResults = allTestable map (resArch.resultsFor) filterNot (_.isEmpty)

    val extantTimeTestablePairs = extantResults map { res =>
      val timings = res map { case (rev, tcr) => tcr.executionTime }
      val avgTiming = timings.foldLeft(0L)({ (sum, time) => sum + time }) / timings.length

      val testable = res.head._2

      (avgTiming, testable)
    }
    val extantTime = extantTimeTestablePairs.foldLeft(0L) { (sum, pair) => sum + pair._1 }


    // if we have sufficent number of results, can compute from that,
    val timeTestablePairs = if (extantTime >= FastTestTime) {
      extantTimeTestablePairs
    } else {
      // otherwise, must run with a short timeout + don't save results (for T/O),
      // so as to see which tests are "quick"

      // Only save results for tests which take 10s or less.
      val QuickTimeout = 10
      val quickConfig = config.copy(saveResultOnTimeout = false,
                                    timeout = QuickTimeout)

      val runHipSleek = new RunHipSleek(quickConfig)
      import runHipSleek.{ altRunTests }

      // If repo is dirty, this will be needlessly expensive.
      val repoC = repo.identify()

      // n.b. *might* throw UnableToBuildException here
      // XXX BUG: It seems this doesn't save the test case result, even for non-timeout?
      val tsr = altRunTests(construct, allTestable)(repoC)

      // wait for the results
      tsr.results map { tcr => (tcr.executionTime, tcr) }
    }


    val sortedTCRs = timeTestablePairs.sortBy { case (time, tc) => time }

    sortedTCRs.foldLeft(List[TestCaseResult]())({ (fastTests, timeTCPair) =>
      val (tcTime, tc) = timeTCPair

      val totalTime = fastTests.foldLeft(0L) { (sum, tcr) => sum + tcr.executionTime }

      if (totalTime < FastTestTime) {
        fastTests :+ tc
      } else {
        fastTests
      }
    })
  }

  private def suiteFromString(suite: String): Suite =
    suite match {
      case "hip" => HipOnly()
      case "sleek" => SleekOnly()
      case "all" => All()
      case "sleek-validate" => SleekValidateOnly()
      case "validate-sleek" => SleekValidateOnly()
      case _ => throw new IllegalArgumentException(s"Unknown suite: $suite")
    }

  def run(): Unit = {
    import config.{ runFastSuite, runFastGenerate => isForcedGenerate }

    val suiteName = runFastSuite.getOrElse(throw new IllegalArgumentException("must be given a suite name"))

    // more/less expect suiteName to be one of:
    //   sleek
    //   hip
    //   all
    //   validate-sleek
    val suite = suiteFromString(suiteName)

    //
    // Step 1. Generate set of fast tests (if need be)
    //

    val fastTests = loadFastTests(suite) match {
      case Some(xs) if !isForcedGenerate => {
        println("Loading fast tests from cached file.")
        xs
      }
      case Some(xs) if isForcedGenerate => {
        println("File exists, but flag forced generation. Generating.")
        val xs = generateFastTestablesForSuite(suite)

        println("Saving cache of fast tests.")
        saveFastTests(suite, xs)

        xs
      }
      case None => {
        println("No fast tests cache file found for this suite. Generating.")
        val xs = generateFastTestablesForSuite(suite)

        println("Saving cache of fast tests.")
        saveFastTests(suite, xs)

        xs
      }
    }

    //
    // Step 2. With the set of generated tests in hand, run these.
    //

    val testable = filterTestable(allTestableForSuite(suite), fastTests map rowFromTestable)

    // TODO RunFast customise revision(?)
    val repoC = repo.identify()

    val runHipSleek = new RunHipSleek(config)
    import runHipSleek.{ altRunTests }

    altRunTests(constructForSuite(suite), testable)(repoC)
  }
}
