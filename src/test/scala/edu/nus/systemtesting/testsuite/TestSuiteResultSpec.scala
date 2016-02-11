package edu.nus.systemtesting.testsuite

import java.nio.file.Path
import org.scalatest.FlatSpec
import edu.nus.systemtesting.ExpectsOutput
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.TestCaseResult

class TestSuiteResultSpec extends FlatSpec {
  "TestSuiteResult" should "throw exception for results (if one TC throws exception)" in {
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

    intercept[IllegalStateException] {
      tsr.results
    }
  }
}