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

object RunHipSleek {
  def foldersUsedFromTestable(testable: List[Testable]): List[String] =
    (testable map { t => t.fileName.getParent().toString() } toSet) toList
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
                  allTestable: List[Testable with ExpectsOutput])
                 (rev: Commit): TestSuiteResult = {
    // Folders used by e.g. SleekTestSuiteUsage, HipTestSuiteUsage
    val foldersUsed = RunHipSleek.foldersUsedFromTestable(allTestable)

    (runTestsWith(rev, foldersUsed) { case (binDir, corpusDir, repoRevision) =>
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
  def runTest(tc: Testable with ExpectsOutput, construct: ConstructTestCase)(rev: Commit): TestCaseResult = {
    val foldersUsed = List(tc.fileName.getParent().toString())

    (runTestsWith(rev, foldersUsed) { case (binDir, corpusDir, repoRevision) =>
      // Ideally, preparedSys would itself do the building of repo.
      // i.e. building the repo would be delayed until necessary.
      // At the moment, though, since any system loading tests will *have* the
      // tests, this is not going to slow things down.
      lazy val preparedSys = PreparedSystem(binDir, corpusDir)

      val resultsFor = runTestCaseForRevision(rev, preparedSys)(construct)

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
    new TestSuite(allTests, repoRevision.revHash, config.significantTimeThreshold)
  }

  // should be able to replace runWith, callback nature with this...
  private[app] def runTestCaseForRevision(repoRevision: Commit, preparedSys: => PreparedSystem)
                                         (implicit construct: ConstructTestCase):
      (Testable with ExpectsOutput => TestCaseResult) = {
    val resultsArch = config.defaultResultsArchive

    { tc: Testable with ExpectsOutput =>
      resultsArch.resultFor(repoRevision)(tc) match {
        case Some(tcr) => tcr

        case None => {
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
      }
    }
  }


  /** For use with `diffSuiteResults`, for running just sleek results. */
  private[app] val sleekResultPairs: (Commit, Commit) => DiffableResults =
    Diff.resultPairsFor("sleek", runSleekTests)

  /** For use with `diffSuiteResults`, for running just hip results. */
  private[app] val hipResultPairs: (Commit, Commit) => DiffableResults =
    Diff.resultPairsFor("hip", runHipTests)
}
