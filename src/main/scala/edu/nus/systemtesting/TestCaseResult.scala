package edu.nus.systemtesting

import java.nio.file.Path

import java.nio.file.Path

/**
 * A system level test will either Pass or Fail.
 */
sealed trait TestResult
case object TestPassed extends TestResult
case object TestFailed extends TestResult

/**
 * @author richardg
 */
case class TestCaseResult(val command: Path,
                          val filename: Path,
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

  val executionSucceeded = results.isRight

  val passed = result == TestPassed

  /** Equivalence val, so that `" --a --b"` is the same as `"--b --a"`. */
  lazy val sortedArgs =
    (arguments trim() split "\\s*" sorted) mkString " "

  def expected() = results match {
    case Left(_) => List[String]()
    case Right(results) => results map(_.expected) toList
  }

  /**
   * Key which almost certain to be unique to a [[TestCaseResult]] in a
   * [[TestSuite]]
   */
  lazy val cmdFnArgsKey =
    (command, filename, sortedArgs)

  /**
   * Key which might be unique to a [[TestCaseResult]] in a [[TestSuite]].
   * Useful for finding tests which differ only in `arguments`.
   */
  lazy val cmdFnKey =
    (command, filename, sortedArgs)
}
