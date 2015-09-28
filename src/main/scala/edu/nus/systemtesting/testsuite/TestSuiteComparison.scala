package edu.nus.systemtesting.testsuite

import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.output.GlobalReporter

import GlobalReporter.reporter

/**
 * For comparing two [[TestSuiteResult]]s. Does not assume that the two
 * results were computed from the same list of [[TestCase]]s, since
 * the list of tests may change over time.
 *
 * Result of the comparison indicates the differences in results of the
 * [[TestSuite]] executions, and notes incompatibilities.
 *
 * Use [[TestSuiteComparison.apply]] to compute the 'diff' of two
 * [[TestSuiteResult]]s.
 *
 * @author richardg
 */
class TestSuiteComparison(val oldRevision: String,
                          val curRevision: String,
                          val argsChangedTests: List[TestCaseDiffPair],
                          val removedTests: List[TestCaseResult],
                          val newTests:     List[TestCaseResult],
                          val nowSuccessfullyRuns:   List[TestCaseDiffPair],
                          val usedToSuccessfullyRun: List[TestCaseDiffPair],
                          val nowPasses:  List[TestCaseDiffPair],
                          val usedToPass: List[TestCaseDiffPair],
                          val diffDiffs:  List[TestCaseDiffPair],
                          val curSlower:  List[TestCaseDiffPair],
                          val curQuicker: List[TestCaseDiffPair]) {
  val unchanged = {
    // unsure about whether curSlower, curQuicker should 'count'
    List(argsChangedTests,
         removedTests,
         newTests,
         nowSuccessfullyRuns,
         usedToSuccessfullyRun,
         nowPasses,
         usedToPass,
         diffDiffs,
         curSlower,
         curQuicker)
      .forall(_.isEmpty)
  }

  def display(name: String): Unit = {
    reporter.header(s"Diff between $name results $oldRevision -> $curRevision")

    if (unchanged) {
      reporter.log("No differences.")
      return
    }

    // Convenient way to display TestCaseResult
    def tcrToString(tcr: TestCaseResult): String = {
      import tcr.{ command, arguments, filename }
      s"TC[$command, $arguments, $filename]"
    }

    if (!argsChangedTests.isEmpty) {
      reporter.log("Args changed:")
      argsChangedTests foreach { case (oldTCR, curTCR) =>
        reporter.log(s"${tcrToString(curTCR)}: args were `${oldTCR.arguments}`")
      }
      reporter.println()
    }

    if (!removedTests.isEmpty) {
      reporter.log("Tests Removed:")
      removedTests foreach { rmTCR =>
        reporter.log(s"- ${tcrToString(rmTCR)}")
      }
      reporter.println()
    }

    if (!newTests.isEmpty) {
      reporter.log("Tests Added:")
      newTests.foreach({ tc =>
        reporter.log(s"+ ${tcrToString(tc)}")
      })
      reporter.println()
    }

    def reportListOfPairs(ls: List[(TestCaseResult, TestCaseResult)], title: String, sym: String = "*"): Unit = {
      if (!ls.isEmpty) {
        reporter.log(s"$title:")
        ls.foreach({ case (_, tc) =>
          // Just show the new TC, not the old one.
          reporter.log(s"$sym ${tcrToString(tc)}")
        })
        reporter.println()
      }
    }

    reportListOfPairs(nowSuccessfullyRuns, "Now runs successfully", "+")
    reportListOfPairs(usedToSuccessfullyRun, "Used to run successfully", "-")

    reportListOfPairs(nowPasses, "Now passes", "+")

    reportListOfPairs(usedToPass, "Used to pass", "-")

    // Might be useful to show the diffs.
    // Might not be best to show this one last, also.
    reportListOfPairs(diffDiffs, "Both fail, but different diffs", "*")
    if (!diffDiffs.isEmpty) {
      reporter.log(s"Fails, but different diffs:")
      diffDiffs.foreach({ case (oldTCR, curTCR) =>
        val oldDiff = oldTCR diff
        val curDiff = curTCR diff

        reporter log s"* ${tcrToString(curTCR)}:"
        reporter log s"Was:"
        oldDiff foreach { res => reporter println res.toString() }
        reporter log s"Now:"
        curDiff foreach { res => reporter println res.toString() }
      })
      reporter.println()
    }

    // Output speed differences
    if (!curSlower.isEmpty) {
      reporter.log(s"Current Tests Slower:")
      curSlower.foreach({ case (oldTCR, curTCR) =>
        val oldT = oldTCR executionTime
        val curT = curTCR executionTime

        reporter log f"* ${tcrToString(curTCR)} was: $oldT%5d ms now: $curT%5d ms"
      })
      reporter.println()
    }

    if (!curQuicker.isEmpty) {
      reporter.log(s"Current Tests Quicker:")
      curQuicker.foreach({ case (oldTCR, curTCR) =>
        val oldT = oldTCR executionTime
        val curT = curTCR executionTime

        reporter log f"* ${tcrToString(curTCR)} was: $oldT%5d ms now: $curT%5d ms"
      })
      reporter.println()
    }
  }
}

object TestSuiteComparison {
  /**
   * Proportion difference between execution times as a threshold for whether a
   * testcase became 'slower' (and, roughly, 'quicker').
   */
  val PerfDiffProportion = 0.5;

  /**
   * Construct a [[TestSuiteComparison]], conveniently computes values for
   * each of the parameters.
   */
  def apply(oldTS: TestSuiteResult, curTS: TestSuiteResult): TestSuiteComparison = {
    // It can't be assumed that oldTS, currTS ran the same set of TestCases
    // (in terms of *SuiteUsage), since these can change over time as TestCases
    // are added, program flags change, etc.
    //
    // Still, roughly assumed that the two SuiteResults are running *roughly*
    // the same set of tests.
    // Somewhat assumes corpusDir is the same, but in theory it could be different
    // (but with same set of files) and it'd work just fine, too.

    val oldRevision = oldTS.repoRevision
    val curRevision = curTS.repoRevision

    //
    // Find which TestCaseResults can be compared, which can't
    //

    // Build Map[(cmd,filename,sortedArgs) => TestCaseResult)] for old,curr
    val oldTests = oldTS.results.toList
    val curTests = curTS.results.toList

    val oldTestMap = oldTests.map(tcr => (tcr.cmdFnArgsKey -> tcr)).toMap
    val curTestMap = curTests.map(tcr => (tcr.cmdFnArgsKey -> tcr)).toMap

    // Partition (Compatible TestCaseResults)
    val compatibleTests = oldTests.map({ oldTCR =>
      curTestMap.get(oldTCR.cmdFnArgsKey).map(curTCR => (oldTCR -> curTCR))
    }).flatten

    val (usedOld, usedCur) = compatibleTests.unzip
    val unusedOld = oldTests.filterNot(usedOld.contains)
    val unusedCur = curTests.filterNot(usedCur.contains)

    val unusedOldMap = unusedOld.map(tcr => (tcr.cmdFnKey -> tcr)).toMap
    val unusedCurMap = unusedCur.map(tcr => (tcr.cmdFnKey -> tcr)).toMap

    // Partition Unused (Different Args)
    val argsChangedTests = unusedOld.map({ oldTCR =>
      // assumes cmdFn key is unique-ish,
      // otherwise `argsChangedTests` is nonsense
      unusedCurMap.get(oldTCR.cmdFnKey).map(curTCR => (oldTCR -> curTCR))
    }).flatten

    // Test is 'removed' if (cmd,file) pair not in new tests.
    // Test is 'new' if (cmd,file) pair not in old tests.
    // (Otherwise, with same args in compatible tests,
    //  with different args in argsChangedTests).
    val (argsChangedOld, argsChangedCur) = argsChangedTests.unzip
    val removedTests = unusedOld.filterNot(argsChangedOld.contains)
    val newTests     = unusedCur.filterNot(argsChangedCur.contains)

    //
    // For the compatible tests:
    //

    // With each (old,curr) TestCaseResult:
    // If both ran successfully AND
    //   if #/seq of Result.expected is different, => ExpectedChanged
    val (successfulPairs, unsuccessfulPairs) = compatibleTests.partition({ case (oldTCR, curTCR) =>
      oldTCR.executionSucceeded && curTCR.executionSucceeded
    })

    // 'comparable' stronger than 'compatible'; it implies same expected.
    val (comparableTests, diffExpectedTests) = successfulPairs.partition({ case (oldTCR, curTCR) =>
      oldTCR.expected == curTCR.expected
    })

    // Change in successfully ran?
    val nowSuccessfullyRuns = unsuccessfulPairs.filter({ case (oldTCR, curTCR) =>
      !oldTCR.executionSucceeded && curTCR.executionSucceeded
    })
    val usedToSuccessfullyRun = unsuccessfulPairs.filter({ case (oldTCR, curTCR) =>
      oldTCR.executionSucceeded && !curTCR.executionSucceeded
    })
    // Remaining (from unsuccessfulPairs): both fail to run.

    // Change in test passing?
    val nowPasses = comparableTests.filter({ case (oldTCR, curTCR) =>
      !oldTCR.passed && curTCR.passed
    })
    val usedToPass = comparableTests.filter({ case (oldTCR, curTCR) =>
      oldTCR.passed && !curTCR.passed
    })
    // Remaining (from comparableTests): results both 'pass' or both 'fail',
    // May be that they fail for different reasons:
    val diffDiffs = comparableTests.filter({ case (oldTCR, curTCR) =>
      (!oldTCR.passed && !curTCR.passed) && (oldTCR.diff != curTCR.diff)
    })

    //
    // Performance differences
    //

    // Use proportion to check whether a test slowed down
    //   e.g. oldT = 1000, curT = 1600,
    //   then (1600 - 1000) / 1000 = 0.6 > 0.5 => "Slower".
    def isTimeSlower(oldT: Long, curT: Long) =
      (curT - oldT).toDouble / oldT > PerfDiffProportion

    val bothPassed = comparableTests filter { case (oldTCR, curTCR) =>
      // Performance diff only matters for passing tests.
      oldTCR.passed && curTCR.passed
    }
    val curSlower = bothPassed filter { case (oldTCR, curTCR) =>
      isTimeSlower(oldTCR.executionTime, curTCR.executionTime)
    }
    val curQuicker = bothPassed filter { case (oldTCR, curTCR) =>
      // This confuses me. Can just swap cur/old into timeSlower
      // e.g. old = 1600, cur = 1000,
      // so, isSlower?(old = cur=1000, cur = old=1600) =
      //       (1600 - 1000) / 1000 = 0.6 > 0.5
      // n.b. Technically, 1600->1000 is ~0.37 decrease, not 50%.
      isTimeSlower(curTCR.executionTime, oldTCR.executionTime)
    }

    // Now construct TestSuiteComparison from the above
    new TestSuiteComparison(
      oldRevision = oldRevision,
      curRevision = curRevision,
      argsChangedTests = argsChangedTests,
      removedTests = removedTests,
      newTests = newTests,
      nowSuccessfullyRuns = nowSuccessfullyRuns,
      usedToSuccessfullyRun = usedToSuccessfullyRun,
      nowPasses = nowPasses,
      usedToPass = usedToPass,
      diffDiffs = diffDiffs,
      curSlower = curSlower,
      curQuicker = curQuicker
    )
  }
}