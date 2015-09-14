package edu.nus.systemtesting.seralisation

import org.scalatest.FlatSpec
import edu.nus.systemtesting.Result
import edu.nus.systemtesting.serialisation.ResultJson
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.serialisation.TestCaseResultJson

/**
 * @author richardg
 */
class JsonSpec extends FlatSpec {
  // test Result to/from,
  "JSON Serialistion" should "be able to encode/decode Result objects" in {
    val res1 = Result("key1", "expected1", "output1")

    val dump = ResultJson.dump(res1)
    val load = ResultJson.load(dump)

    load match {
      case None => fail()
      case Some(res) => {
        assert(res1 equals res)
      }
    }
  }

  // test TestCaseResult to/from
  it should "be able to encode/decode TestCaseResult objects" in {
    val res1 = Result("key1", "expected1", "output1")
    val testCaseResult1 = new TestCaseResult("cmd1", "filename1", "--args", 10L, Left(List("remark1", "remark2")))

    val dump = TestCaseResultJson.dump(testCaseResult1)
    val load = TestCaseResultJson.load(dump)

    load match {
      case None => fail()
      case Some(res) => {
        assert(testCaseResult1 equals res, s"Couldn't load from:\n$dump\nGot: $res")
      }
    }

    val testCaseResult2 = new TestCaseResult("cmd1", "filename1", "--args", 10L, Right(List(res1)))

    val dump2 = TestCaseResultJson.dump(testCaseResult2)
    val load2 = TestCaseResultJson.load(dump2)

    load2 match {
      case None => fail()
      case Some(res) => {
        assert(testCaseResult2 equals res, s"Couldn't load from:\n$dump2\nGot: $res")
      }
    }
  }
}