package edu.nus.systemtesting.testsuite

import java.nio.file.Paths

import org.joda.time.DateTime
import org.scalatest.FlatSpec

import edu.nus.systemtesting.Result
import edu.nus.systemtesting.TestCaseResult

class TestSuiteComparisonSpec extends FlatSpec {
  // Need to check
  //   argsChangedTests
  //   removedTests + newTests
  //   nowSuccessfullyRuns + usedToSuccessfullyRun
  //   nowPasses + usedToPass
  //   diffDiffs

  val arbitraryHostname = "soccf-plser2-05"
  val arbitraryDateTime = DateTime.now()

  def results(rev: String, tcResults: Iterable[TestCaseResult]): TestSuiteResult = {
    TestSuiteResult.withResults(
                    arbitraryHostname,
                    arbitraryDateTime,
                    rev,
                    tcResults.toList)
  }

  val arbitraryCommand = Paths.get("cmd")

  val TimeQuick = 1000L
  val TimeSuperQuick = 100L
  val TimeSlower = 2000L

  /**
   * TestCaseResult with successful execution.
   */
  def tcRes(filename: String,
            args: String,
            results: Iterable[(String, String)],
            time: Long = TimeQuick): TestCaseResult = {
    TestCaseResult(arbitraryCommand,
                   Paths.get(filename),
                   args,
                   time,
                   Right(results.zipWithIndex.map({ case ((k, v), idx) =>
                     Result(idx.toString, k, v)
                   })))
  }

  // TestCaseResult with unsuccessful execution
  def tcResErr(filename: String,
               args: String,
               time: Long = TimeQuick): TestCaseResult = {
    TestCaseResult(arbitraryCommand,
                   Paths.get(filename),
                   args,
                   time,
                   Left(List("error")))
  }

  // A 'basic' TestSuiteResult, which is easy to check if things improve from.
  val ControlResult = results("R1", List(
    tcRes("test1", "", List("a" -> "a", "b" -> "b")),
    tcRes("test2", "", List("a" -> "a", "b" -> "a"))
  ))

  val ResultWithArgsChanged = results("R1", List(
    tcRes("test1", "--arg1", List("a" -> "a", "b" -> "b")),
    tcRes("test2", "", List("a" -> "a", "b" -> "a"))
  ))

  val ResultWithTestRemoved = results("R1", List(
    tcRes("test1", "", List("a" -> "a", "b" -> "b"))
  ))

  val ResultWithTestAdded = results("R1", List(
    tcRes("test1", "", List("a" -> "a", "b" -> "b")),
    tcRes("test2", "", List("a" -> "a", "b" -> "a")),
    tcRes("test3", "", List("a" -> "a", "b" -> "b"))
  ))

  // n.b. not derive from ControlResult
  val ResultWithSucErr = results("R1", List(
    tcRes("test1", "", List("a" -> "a", "b" -> "b")),
    tcResErr("test2", ""),
    tcResErr("test3", "")
  ))

  // n.b. not derive from ControlResult
  val ResultWithErrSuc = results("R1", List(
    tcResErr("test1", ""),
    tcRes("test2", "", List("a" -> "a", "b" -> "b")),
    tcRes("test3", "", List("a" -> "a", "b" -> "a"))
  ))

  // test1 now fails, test2 now passes
  val ResultWithDiffResults = results("R1", List(
    tcRes("test1", "", List("a" -> "a", "b" -> "a")),
    tcRes("test2", "", List("a" -> "a", "b" -> "b"))
  ))

  // test2 still fails, but now differently
  val ResultWithDiffDiffs = results("R1", List(
    tcRes("test1", "", List("a" -> "a", "b" -> "b")),
    tcRes("test2", "", List("a" -> "b", "b" -> "b"))
  ))

  // TODO: MUCH SLOWER.. / QUICKER..
  val ResultWithSlower = results("R1", List(
    tcRes("test1", "", List("a" -> "a", "b" -> "b"), TimeSlower),
    tcRes("test2", "", List("a" -> "a", "b" -> "a"))
  ))

  val ResultWithQuicker = results("R1", List(
    tcRes("test1", "", List("a" -> "a", "b" -> "b"), TimeSuperQuick),
    tcRes("test2", "", List("a" -> "a", "b" -> "a"))
  ))



  //   argsChangedTests
  "TestSuiteResult Diff" should "compute diff for args changed" in {
    val diff = TestSuiteComparison("test", ControlResult, ResultWithArgsChanged)

    // dupl. from above
    val oldTC = tcRes("test1", "", List("a" -> "a", "b" -> "b"))
    val curTC = tcRes("test1", "--arg1", List("a" -> "a", "b" -> "b"))
    val expected = List(oldTC -> curTC)

    assertResult(expected)(diff.argsChangedTests)
  }

  it should "compute no diff for args changed (when absent)" in {
//    assert(TestSuiteComparison(ControlResult, ResultWithArgsChanged).argsChangedTests.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestAdded).argsChangedTests.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestRemoved).argsChangedTests.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffResults).argsChangedTests.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).argsChangedTests.isEmpty)
  }


  //   removedTests + newTests
  it should "compute diff for when tests added, removed" in {
    val diff1 = TestSuiteComparison("test", ControlResult, ResultWithTestRemoved)
    assert(diff1.removedTests.length == 1)

    val diff2 = TestSuiteComparison("test", ControlResult, ResultWithTestAdded)
    assert(diff2.newTests.length == 1)
  }

  it should "compute no diff for when tests added, removed (if no change)" in {
    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).removedTests.isEmpty)
//    assert(TestSuiteComparison(ControlResult, ResultWithTestAdded).removedTests.isEmpty)
//    assert(TestSuiteComparison(ControlResult, ResultWithTestRemoved).removedTests.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffResults).removedTests.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).removedTests.isEmpty)

    // Could just keep the diffs above, if this is very slow.
    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).newTests.isEmpty)
//    assert(TestSuiteComparison(ControlResult, ResultWithTestAdded).newTests.isEmpty)
//    assert(TestSuiteComparison(ControlResult, ResultWithTestRemoved).newTests.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffResults).newTests.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).newTests.isEmpty)
  }


  //   nowSuccessfullyRuns + usedToSuccessfullyRun
  it should "compute diff for change in test execution ability" in {
    // goes from 1pass, 2fail -> 1fail, 2pass
    val diff = TestSuiteComparison("test", ResultWithSucErr, ResultWithErrSuc)

    assert(diff.nowSuccessfullyRuns.length == 2)
    assert(diff.usedToSuccessfullyRun.length == 1)
  }

  it should "compute no diff for change in test execution ability (if no change)" in {
    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).nowSuccessfullyRuns.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestAdded).nowSuccessfullyRuns.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestRemoved).nowSuccessfullyRuns.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffResults).nowSuccessfullyRuns.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).nowSuccessfullyRuns.isEmpty)

    // Could just keep the diffs above, if this is very slow.
    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).usedToSuccessfullyRun.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestAdded).usedToSuccessfullyRun.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestRemoved).usedToSuccessfullyRun.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffResults).usedToSuccessfullyRun.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).usedToSuccessfullyRun.isEmpty)
  }


  //   nowPasses + usedToPass
  it should "compute diff for change in test pass/fail" in {
    // swith around from pass, fail -> fail, pass
    val diff = TestSuiteComparison("test", ControlResult, ResultWithDiffResults)

    assert(diff.nowPasses.length == 1)
    assert(diff.usedToPass.length == 1)
  }

  it should "compute no diff for change in test pass/fail (if no change)" in {
    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).nowPasses.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestAdded).nowPasses.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestRemoved).nowPasses.isEmpty)
//    assert(TestSuiteComparison(ControlResult, ResultWithDiffResults).nowPasses.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).nowPasses.isEmpty)

    // Could just keep the diffs above, if this is very slow.
    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).usedToPass.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestAdded).usedToPass.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestRemoved).usedToPass.isEmpty)
//    assert(TestSuiteComparison(ControlResult, ResultWithDiffResults).usedToPass.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).usedToPass.isEmpty)
  }


  //   diffDiffs
  it should "compute diff for change in diffs" in {
    // changes all the results of the 2nd testcase. (1x diff diff)
    val diff = TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs)

    assert(diff.diffDiffs.length == 1)
  }

  it should "compute no diff for change in diffs (if no change)" in {
    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).diffDiffs.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestAdded).diffDiffs.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestRemoved).diffDiffs.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffResults).diffDiffs.isEmpty)
//    assert(TestSuiteComparison(ControlResult, ResultWithDiffDiffs).diffDiffs.isEmpty)
  }


  //   performance Diffs
  it should "compute diff for slower tests" in {
    val diff = TestSuiteComparison("test", ControlResult, ResultWithSlower)

    assert(diff.curSlower.length == 1)
  }

  it should "compute diff for quicker tests" in {
    val diff = TestSuiteComparison("test", ControlResult, ResultWithQuicker)

    assert(diff.curQuicker.length == 1)
  }

  it should "compute no diff for perfomance diffs (if no change)" in {
    // I feel that this isn't strong enough; should also test that tests which
    // only slightly quicker should be no-change..
    assert(TestSuiteComparison("test", ControlResult, ResultWithSlower).curQuicker.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithQuicker).curSlower.isEmpty)

    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).curSlower.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestAdded).curSlower.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestRemoved).curSlower.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffResults).curSlower.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).curSlower.isEmpty)

    assert(TestSuiteComparison("test", ControlResult, ResultWithArgsChanged).curQuicker.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestAdded).curQuicker.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithTestRemoved).curQuicker.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffResults).curQuicker.isEmpty)
    assert(TestSuiteComparison("test", ControlResult, ResultWithDiffDiffs).curQuicker.isEmpty)
  }
}