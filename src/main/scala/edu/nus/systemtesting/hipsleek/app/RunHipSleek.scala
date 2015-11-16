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
                 "sleek",
                 SleekTestSuiteUsage.allTestable)(rev),
     altRunTests(HipTestCase.constructTestCase,
                 "hip",
                 HipTestSuiteUsage.allTestable)(rev))

  private[app] def runHipTests(rev: Commit): TestSuiteResult =
    altRunTests(HipTestCase.constructTestCase,
                "hip",
                HipTestSuiteUsage.allTestable)(rev)

  private[app] def runSleekTests(rev: Commit): TestSuiteResult =
    altRunTests(SleekTestCase.constructTestCase,
                "sleek",
                SleekTestSuiteUsage.allTestable)(rev)

  // construct e.g. HipTestCase.constructTestCase
  def altRunTests(construct: (PreparedSystem, Testable, TestCaseConfiguration) => TestCase,
                  suiteName: String,
                  allTestable: List[Testable])
                 (rev: Commit): TestSuiteResult = {
    (runTestsWith(rev, "examples/working/" + suiteName) { case (binDir, corpusDir, repoRevision) =>
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
        throw new UnableToBuildException(repoDir, Some(rev.revHash))
      case BuildTimedOut() =>
        throw new UnableToBuildException(repoDir, Some(rev.revHash), timedOut = true)
    }
  }

  private def suiteFor(allTests: List[Testable], repoRevision: Commit): TestSuite = {
    // XXX: This *ignores* dirty?
    new TestSuite(allTests, repoRevision.revHash, config.significantTimeThreshold)
  }

  // should be able to replace runWith, callback nature with this...
  private[app] def runTestCaseForRevision(repoRevision: Commit, preparedSys: => PreparedSystem)
                                         (implicit construct: (PreparedSystem, Testable, TestCaseConfiguration) => TestCase):
      (Testable => TestCaseResult) = {
    val resultsArch = new ResultsArchive(config.resultsDir, config.buildFailuresFile)

    { tc: Testable =>
      resultsArch.resultFor(repoRevision.revHash)(tc) match {
        case Some(tcr) => tcr

        case None => {
          val conf = TestCaseConfiguration(timeout = config.timeout)
          val tcase = construct(preparedSys, tc, conf)

          // Run the TestCase, save results
          val tcr = tcase.generateOutput

          // Didn't have results, save them.
          try {
            resultsArch.saveTestCaseResult(repoRevision.revHash, tcr)
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
