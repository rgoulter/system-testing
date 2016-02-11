package edu.nus.systemtesting.seralisation

import java.nio.file.Paths

import org.joda.time.DateTime
import org.scalatest.FlatSpec

import edu.nus.systemtesting.Result
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.serialisation.Json
import edu.nus.systemtesting.serialisation.ResultJson
import edu.nus.systemtesting.serialisation.TestCaseResultJson
import edu.nus.systemtesting.testsuite.TestSuiteResult

/**
 * @author richardg
 */
class JsonSpec extends FlatSpec {
  def checkIdempotent[T](t : T, json : Json[T]) {
    val dump = json.dump(t)
    val load = json.load(dump)

    load match {
      case None => fail()
      case Some(res) => {
        assert(t equals res, s"Couldn't load from:\n$dump\nGot: $res")
      }
    }
  }

  val cmd1 = Paths.get("cmd1")
  val filename1 = Paths.get("filename1")

  val SampleResult = Result("key1", "expected1", "output1")
  val SampleTestCaseResult1 = new TestCaseResult(cmd1, filename1, "--args", 10L, Left(List("remark1", "remark2")))
  val SampleTestCaseResult2 = new TestCaseResult(cmd1, filename1, "--args", 10L, Right(List(SampleResult)))
  val SampleTestSuiteResult = TestSuiteResult.withResults("soccf-plser2-05", DateTime.now(), "abcd", List(SampleTestCaseResult1, SampleTestCaseResult2))

  // test Result to/from,
  "JSON Serialistion" should "be able to encode/decode Result objects" in {
    checkIdempotent(SampleResult, ResultJson)
  }

  // test TestCaseResult to/from
  it should "be able to encode/decode TestCaseResult objects" in {
    checkIdempotent(SampleTestCaseResult1, TestCaseResultJson)

    checkIdempotent(SampleTestCaseResult2, TestCaseResultJson)
  }
}