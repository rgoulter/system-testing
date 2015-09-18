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
  def apply(oldTS: TestSuiteResult, currTS: TestSuiteResult) {
    // It can't be assumed that oldTS, currTS ran the same set of TestCases
    // (in terms of *SuiteUsage), since these can change over time as TestCases
    // are added, program flags change, etc.
    //
    // Still, roughly assumed that the two SuiteResults are running *roughly*
    // the same set of tests.
    // Somewhat assumes corpusDir is the same, but in theory it could be different
    // (but with same set of files) and it'd work just fine, too.

    val oldRevision = oldTS.repoRevision
    val currRevision = currTS.repoRevision

    //
    // Find which TestCaseResults can be compared, which can't
    //

    // Build Map[(cmd,filename,sortedArgs) => TestCaseResult)] for old,curr

    // Partition into:
    // * compatible (old,curr)
    // * unmatched old,
    // * unused curr

    // DiffArgs for those with same (cmd,filename) in unmatched old, unused curr
    //   (assuming that (cmd,filename) is unique-ish)
    // TestRemoved if (cmd,filename) in old, not in curr,
    // NewTest     if (cmd,filename) not in old, in curr

    // For the compatible,

    // With each (old,curr) TestCaseResult:
    // If both ran successfully AND
    //   if #/seq of Result.expected is different, => ExpectedChanged

    // Otherwise, the DIFFERENT results will be:
    // (failedToRun, successfullyRun) # GOOD
    // (successfullyRun, failedToRun) # BAD
    // (testcase failed, testcase passed) # GOOD
    // (testcase passed, testcase failed) # BAD

    // What remains is "not different";
    // Can print summary of the currRes if need be (to see #passed, #failed)
  }
}