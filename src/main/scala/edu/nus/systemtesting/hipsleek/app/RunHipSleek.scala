package edu.nus.systemtesting.hipsleek.app

import edu.nus.systemtesting.ConstructTestCase
import edu.nus.systemtesting.ExpectsOutput
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.TestCaseConfiguration
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hipsleek.BuildFailed
import edu.nus.systemtesting.hipsleek.BuildTimedOut
import edu.nus.systemtesting.hipsleek.HipTestCase
import edu.nus.systemtesting.hipsleek.HipTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SleekTestCase
import edu.nus.systemtesting.hipsleek.SleekTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.hipsleek.TestSuiteResultAnalysis
import edu.nus.systemtesting.testsuite.TestSuite
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.ExpectsOutput

/** Used with runTests, if specified to load from result, but result not found. */
class UnableToRunException(val rev: Commit, tc: Testable with ExpectsOutput)
  extends RuntimeException(s"Could not run for revision ${rev.revHash} for $tc")

object RunHipSleek {
  def foldersUsedFromTestable(testable: List[Testable]): List[String] =
    (testable map { t => t.fileName.getParent().toString() } toSet) toList

  /** Used with runTests, for whether to only load results, to always run results, or to run tests only if no results available. */
  case class RunTestsMethod(val loadResults: Boolean, val runSystem: Boolean)
}

/**
 * @author richardg
 */
class RunHipSleek(config: AppConfig) extends UsesRepository(config) {
  // Easier to split logic-heavy parts of main into other classes
  val runUtils = new RunUtils(config)
  import runUtils.runTestsWith


  private[app] def runHipTests(rev: Commit): TestSuiteResult =
    runTests(HipTestCase.constructTestCase,
             HipTestSuiteUsage.allTestable)(rev)

  private[app] def runSleekTests(rev: Commit): TestSuiteResult =
    runTests(SleekTestCase.constructTestCase,
             SleekTestSuiteUsage.allTestable)(rev)

  // construct e.g. HipTestCase.constructTestCase
  def runTests(construct: ConstructTestCase,
               allTestable: List[Testable with ExpectsOutput],
               runTestsMethod: RunHipSleek.RunTestsMethod = RunHipSleek.RunTestsMethod(true, true))
              (rev: Commit): TestSuiteResult = {
    // Folders used by e.g. SleekTestSuiteUsage, HipTestSuiteUsage
    val foldersUsed = RunHipSleek.foldersUsedFromTestable(allTestable)

    (runTestsWith(rev, foldersUsed) { case (binDir, corpusDir, repoRevision) =>
      // Ideally, preparedSys would itself do the building of repo.
      // i.e. building the repo would be delayed until necessary.
      // At the moment, though, since any system loading tests will *have* the
      // tests, this is not going to slow things down.
      lazy val preparedSys = PreparedSystem(binDir, corpusDir)

      val resultsFor = runTestsMethod match {
        case RunHipSleek.RunTestsMethod(true, true) =>
          runTestCaseForRevision(repoRevision, preparedSys, construct)(_)
        case RunHipSleek.RunTestsMethod(false, true) =>
          runTestCaseForRevisionOnly(repoRevision, preparedSys, construct)(_)
        case RunHipSleek.RunTestsMethod(true, false) =>
          tryLoadTestCaseForRevisionOnly(repoRevision)(_)
        case _ =>
          throw new IllegalArgumentException("Invalid argument, must use at least one method to load from " + runTestsMethod)
      }

      val suite = suiteFor(allTestable, repoRevision)

      // runPrepared just gets TSuite from *TSUsage, then calls runAll w/ archive, rtn result
      val testSuiteResult = suite.runAllTests(resultsFor)

      // displayResult blocks until all results computed
      testSuiteResult.displayResult(config.significantTimeThreshold)
      TestSuiteResultAnalysis printTallyOfInvalidTests testSuiteResult

      testSuiteResult
    }) match {
      case SuccessfulBuildResult(tsr) => tsr
      case BuildFailed() =>
        throw new UnableToBuildException(repoDir, rev)
      case BuildTimedOut() =>
        throw new UnableToBuildException(repoDir, rev, timedOut = true)
    }
  }

  // n.b. this exports archive to tmpDir each time, either to build, or
  // just for the examples.
  def runTest(tc: Testable with ExpectsOutput,
              construct: ConstructTestCase,
              runTestsMethod: RunHipSleek.RunTestsMethod = RunHipSleek.RunTestsMethod(true, true))
             (rev: Commit): TestCaseResult = {
    val foldersUsed = List(tc.fileName.getParent().toString())

    (runTestsWith(rev, foldersUsed) { case (binDir, corpusDir, repoRevision) =>
      // Ideally, preparedSys would itself do the building of repo.
      // i.e. building the repo would be delayed until necessary.
      // At the moment, though, since any system loading tests will *have* the
      // tests, this is not going to slow things down.
      lazy val preparedSys = PreparedSystem(binDir, corpusDir)

      val resultsFor = runTestsMethod match {
        case RunHipSleek.RunTestsMethod(true, true) =>
          runTestCaseForRevision(repoRevision, preparedSys, construct)(_)
        case RunHipSleek.RunTestsMethod(false, true) =>
          runTestCaseForRevisionOnly(repoRevision, preparedSys, construct)(_)
        case RunHipSleek.RunTestsMethod(true, false) =>
          tryLoadTestCaseForRevisionOnly(repoRevision)(_)
        case _ =>
          throw new IllegalArgumentException("Invalid argument, must use at least one method to load from " + runTestsMethod)
      }

      // by this point,
      // tc *must* have proper expectedOutput
      resultsFor(tc)
    }) match {
      case SuccessfulBuildResult(tcr) => tcr
      case BuildFailed() =>
        throw new UnableToBuildException(repoDir, rev)
      case BuildTimedOut() =>
        throw new UnableToBuildException(repoDir, rev, timedOut = true)
    }
  }

  private def suiteFor(allTests: List[Testable with ExpectsOutput], repoRevision: Commit): TestSuite = {
    // n.b. Technically this ignores 'dirty', since revHash doesn't include the `+`,
    // but since the only effect is for the cache folders (elsewhere), doesn't
    // matter too much here.
    new TestSuite(allTests, repoRevision.revHash)
  }

  // should be able to replace runWith, callback nature with this...
  private[app] def runTestCaseForRevision(repoRevision: Commit,
                                          preparedSys: => PreparedSystem,
                                          construct: ConstructTestCase)
                                         (tc: Testable with ExpectsOutput):
      TestCaseResult = {
    try {
      tryLoadTestCaseForRevisionOnly(repoRevision)(tc)
    } catch {
      case e: UnableToRunException =>
        runTestCaseForRevisionOnly(repoRevision, preparedSys, construct)(tc)
    }
  }

  /** Try to load using results archive; throw UnableToRunException if result not found. */
  private[app] def tryLoadTestCaseForRevisionOnly(repoRevision: Commit)(tc: Testable with ExpectsOutput):
      TestCaseResult = {
    val resultsArch = config.defaultResultsArchive

    resultsArch.resultFor(repoRevision)(tc) match {
      case Some(tcr) => tcr
      case None => throw new UnableToRunException(repoRevision, tc)
    }
  }

  private[app] def runTestCaseForRevisionOnly(repoRevision: Commit,
                                              preparedSys: => PreparedSystem,
                                              construct: ConstructTestCase)
                                             (tc: Testable with ExpectsOutput):
      TestCaseResult = {
    val resultsArch = config.defaultResultsArchive

    val conf = TestCaseConfiguration(timeout = config.timeout)
    val tcase = construct(preparedSys, tc, conf)

    // Run the TestCase, save results
    val tcr = tcase.generateOutput

    // Didn't have results, save them.
    try {
      val isTimedOut = tcr.executionTime >= config.timeout
      val canSave = config.saveResultOnTimeout || !isTimedOut

      if (canSave) {
        resultsArch.saveTestCaseResult(repoRevision, tcr)
      }
    } catch {
      case e: Throwable => {
        e.printStackTrace()
      }
    }

    tcr
  }

  /** For use with `diffSuiteResults`, for running just sleek results. */
  private[app] val sleekResultPairs: (Commit, Commit) => DiffableResults =
    Diff.resultPairsFor("sleek", runSleekTests)

  /** For use with `diffSuiteResults`, for running just hip results. */
  private[app] val hipResultPairs: (Commit, Commit) => DiffableResults =
    Diff.resultPairsFor("hip", runHipTests)
}
