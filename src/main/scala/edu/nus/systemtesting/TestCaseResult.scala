package edu.nus.systemtesting

/**
 * A system level test will either Pass or Fail.
 */
sealed trait TestResult
case object TestPassed extends TestResult
case object TestFailed extends TestResult

/**
 * @author richardg
 */
class TestCaseResult(val command : String,
                     val filename : String,
                     val arguments : String,
                     val executionOutput : ExecutionOutput,
                     val executionTime : Long,
                     val result : TestResult,
                     val diff : Iterable[(String, String)] = Seq(),
                     val remarks : Iterable[String] = Seq()) {
}