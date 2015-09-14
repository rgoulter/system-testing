package edu.nus.systemtesting.seralisation

import org.scalatest.FlatSpec
import edu.nus.systemtesting.Result
import edu.nus.systemtesting.serialisation.ResultJson
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.serialisation.TestCaseResultJson
import edu.nus.systemtesting.serialisation.TestCaseResultImplicits
import edu.nus.systemtesting.serialisation.Json

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

  // test Result to/from,
  "JSON Serialistion" should "be able to encode/decode Result objects" in {
    val res1 = Result("key1", "expected1", "output1")

    checkIdempotent(res1, ResultJson)
  }

  // test TestCaseResult to/from
  it should "be able to encode/decode TestCaseResult objects" in {
    val res1 = Result("key1", "expected1", "output1")
    val testCaseResult1 = new TestCaseResult("cmd1", "filename1", "--args", 10L, Left(List("remark1", "remark2")))

    checkIdempotent(testCaseResult1, TestCaseResultJson)

    val testCaseResult2 = new TestCaseResult("cmd1", "filename1", "--args", 10L, Right(List(res1)))

    checkIdempotent(testCaseResult2, TestCaseResultJson)
  }
}