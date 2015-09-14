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
case class TestCaseResult(val command: String,
                          val filename: String,
                          val arguments: String,
                          val executionTime: Long,
                          val results: Either[Iterable[String], Iterable[Result]]) {
  val result: TestResult = results match {
    case Left(_) => TestFailed
    case Right(res) => if (res.forall(_.passed)) TestPassed else TestFailed
  }

  val diff: Array[Result] = results match {
    // Doesn't make sense to ask for diff in case of Left
    case Left(_) => Array[Result]()
    case Right(resultUnits) =>
      resultUnits.filterNot(_.passed).toArray
  }

  val remarks: Array[String] = results match {
    case Left(rem) => rem.toArray
    // Doesn't make sense to ask for remarks in case of Right
    case Right(_) => Array[String]()
  }
}
