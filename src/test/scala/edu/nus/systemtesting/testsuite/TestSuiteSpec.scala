package edu.nus.systemtesting.testsuite

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Timer
import java.util.TimerTask
import org.scalatest.BeforeAndAfter
import org.scalatest.FlatSpec
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigFactory
import edu.nus.systemtesting.ExpectsOutput
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.TestCaseResult

/**
 * @author richardg
 */
class TestSuiteSpec extends FlatSpec {

  "TestSuite" should "throw an exception if any resultFor throws an exception" in {
    // Values unused by TestSuite
    val ArbitraryRev = "rev1"

    // Need non-empty list of tests.
    // But, can each be null, since TestSuite doesn't consider them.
    val tests: List[Testable with ExpectsOutput] = List(null, null, null)

    val testSuite = new TestSuite(tests, ArbitraryRev)

    // Deliberately throw some exception,
    // which TestSuite.runAllTests is unlikely to throw.
    def resultFor(tc: Testable with ExpectsOutput): TestCaseResult =
      throw new IllegalStateException("")

    // Need to fail after some time,
    // since that indicates that TestSuite got 'stuck' somewhere.
    val timer = new Timer("TestSuiteSpecTimer")
    val Timeout = 1000L
    timer.schedule(new TimerTask() {
      override def run(): Unit = {
        fail()
      }
    }, Timeout)

    // runAllTests asynchronously returns TestSuiteResult, so won't throw exception.
    val tsr = testSuite.runAllTests(resultFor)

    // Exception *should* be thrown when awaiting the TCRs of the TSR.
    intercept[IllegalStateException] {
      tsr.results
    }

    // cancel timer.
    timer.cancel()
  }
}