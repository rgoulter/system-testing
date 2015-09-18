package edu.nus.systemtesting.testsuite

/**
 * For comparing two [[TestSuiteResult]]s. Does not assume that the two
 * results were computed from the same list of [[TestCase]]s, since
 * the list of tests may change over time.
 *
 * Result of the comparison indicates the differences in results of the
 * [[TestSuite]] executions, and notes incompatibilities.
 *
 * @author richardg
 */
class TestSuiteComparison() {
}

object TestSuiteComparison {
  def apply(oldTS: TestSuiteResult, curTS: TestSuiteResult) {
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
    val diffDiffs = comparableTests.filter({case (oldTCR, curTCR) =>
      (!oldTCR.passed && !curTCR.passed) && (oldTCR.diff != curTCR.diff)
    })

    // Now construct TestSuiteComparison from the above
  }
}