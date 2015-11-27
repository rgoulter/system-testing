package edu.nus.systemtesting.hipsleek.app

import java.nio.file.Path
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseConfiguration
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.hipsleek.BuildFailed
import edu.nus.systemtesting.hipsleek.BuildTimedOut
import edu.nus.systemtesting.hipsleek.HipTestCase
import edu.nus.systemtesting.hipsleek.HipTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SleekTestCase
import edu.nus.systemtesting.hipsleek.SleekTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.hipsleek.TestSuiteResultAnalysis
import edu.nus.systemtesting.serialisation.ResultsArchive
import edu.nus.systemtesting.testsuite.TestSuite
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.ExpectsOutput
import edu.nus.systemtesting.ExpectsOutput

object RunHipSleek {
  def foldersUsedFromTestable(testable: List[Testable]): List[String] =
    (testable map { t => t.fileName.getParent().toString() } toSet) toList
}

/**
 * @author richardg
 */
class RunHipSleek(config: AppConfig) {
  val repoDir: Path = config.repoDirOrDie

  // Each instance of `ConfiguredMain` only ever uses the one `Repository`
  val repo = new Repository(repoDir)


  // Easier to split logic-heavy parts of main into other classes
  val runUtils = new RunUtils(config)
  import runUtils.runTestsWith


  private[app] def runAllTests(rev: Commit): (TestSuiteResult, TestSuiteResult) =
    (altRunTests(SleekTestCase.constructTestCase,
                 SleekTestSuiteUsage.allTestable)(rev),
     altRunTests(HipTestCase.constructTestCase,
                 HipTestSuiteUsage.allTestable)(rev))

  private[app] def runHipTests(rev: Commit): TestSuiteResult =
    altRunTests(HipTestCase.constructTestCase,
                HipTestSuiteUsage.allTestable)(rev)

  private[app] def runSleekTests(rev: Commit): TestSuiteResult =
    altRunTests(SleekTestCase.constructTestCase,
                SleekTestSuiteUsage.allTestable)(rev)

  // construct e.g. HipTestCase.constructTestCase
  def altRunTests(construct: (PreparedSystem, Testable with ExpectsOutput, TestCaseConfiguration) => TestCase,
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

  private def suiteFor(allTests: List[Testable with ExpectsOutput], repoRevision: Commit): TestSuite = {
    // n.b. Technically this ignores 'dirty', since revHash doesn't include the `+`,
    // but since the only effect is for the cache folders (elsewhere), doesn't
    // matter too much here.
    new TestSuite(allTests, repoRevision.revHash, config.significantTimeThreshold)
  }

  // should be able to replace runWith, callback nature with this...
  private[app] def runTestCaseForRevision(repoRevision: Commit, preparedSys: => PreparedSystem)
                                         (implicit construct: (PreparedSystem, Testable with ExpectsOutput, TestCaseConfiguration) => TestCase):
      (Testable with ExpectsOutput => TestCaseResult) = {
    val resultsArch = new ResultsArchive(config.resultsDir, config.buildFailuresFile)

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
}
