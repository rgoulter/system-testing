package edu.nus.systemtesting.testsuite

import java.nio.file.Path
import org.scalatest.FlatSpec
import edu.nus.systemtesting.ExpectsOutput
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.TestCaseResult

class FakeTC(val x: String) extends Testable with ExpectsOutput {
  override val commandName: Path = null
  override val fileName: Path = null
  override val arguments: String = ""
  override val expectedOutput: String = ""

  lazy val result: TestCaseResult =
    new TestCaseResult(commandName, fileName, arguments, 0L, Left(List(x)))
}

class PartialTestSuiteResultSpec extends FlatSpec {
  "Partial TSR" should "evaluate all non-failing TestCaseResults" in {
    // Values unused by TestSuite
    val ArbitraryRev = "rev1"

    val okTC = new FakeTC("ok")
    val badTC = new FakeTC("bad") // will 
    val tests: List[Testable with ExpectsOutput] = List(okTC, badTC, okTC)

    val testSuite = new TestSuite(tests, ArbitraryRev)

    // Deliberately throw some exception,
    // which TestSuite.runAllTests is unlikely to throw.
    def resultFor(tc: Testable with ExpectsOutput): TestCaseResult = {
      // Only throw if the fake-TC message is "bad".
      tc match {
        case tc: FakeTC if tc.x != "bad" => tc.result
        case _ => throw new IllegalStateException()
      }
    }

    // runAllTests asynchronously returns TestSuiteResult
    val tsr = testSuite.runAllTests(resultFor)

    val safeTSR = new PartialTestSuiteResult(tsr)
    val safeResults = safeTSR.results

    assert(safeResults.length == 2)
  }
}