package edu.nus.systemtesting.serialisation

import argonaut._, Argonaut._
import edu.nus.systemtesting.Result
import edu.nus.systemtesting.TestCaseResult

/**
 * Argonaut EncodeJson, DecodeJson implicits for [[Result]]
 * @author richardg
 */
trait ResultImplicits {
  private val Key = "key"
  private val Expected = "expected"
  private val Actual = "actual"

  implicit def ResultEncodeJson: EncodeJson[Result] =
    EncodeJson((r: Result) =>
      (Key      := r.key) ->:
      (Expected := r.expected) ->:
      (Actual   := r.actual) ->:
      jEmptyObject)

  implicit def ResultDecodeJson: DecodeJson[Result] =
    DecodeJson(c => for {
      key      <- (c --\ Key).as[String]
      expected <- (c --\ Expected).as[String]
      actual   <- (c --\ Actual).as[String]
    } yield new Result(key, expected, actual))
}

/**
 * Argonaut EncodeJson, DecodeJson implicits for [[TestCaseResult]]
 * @author richardg
 */
trait TestCaseResultImplicits extends ResultImplicits {
  private val Command = "command"
  private val Filename = "filename"
  private val Arguments = "arguments"
  private val ExecutionTime = "executionTime"
  private val Remarks = "remarks"
  private val Results = "results"

  implicit def TestCaseResultEncodeJson: EncodeJson[TestCaseResult] =
    EncodeJson((tcr: TestCaseResult) => {
      val base = Json(Command       := tcr.command,
                      Filename      := tcr.filename,
                      Arguments     := tcr.arguments,
                      ExecutionTime := tcr.executionTime)
      tcr.results match {
        case Left(remarks)  => (Remarks := remarks.toList) ->: base
        case Right(results) => (Results := results.toList) ->: base
      }
    })

  implicit def TestCaseResultDecodeJson: DecodeJson[TestCaseResult] =
    DecodeJson(c => {
      // First match for the compulsory properties
      val baseDecode = for {
        cmd      <- (c --\ Command).as[String]
        filename <- (c --\ Filename).as[String]
        args     <- (c --\ Arguments).as[String]
        time     <- (c --\ ExecutionTime).as[Long]
      } yield (cmd, filename, args, time)

      baseDecode.toEither match {
        // Some properties from base not present
        case Left((message, history)) =>
          DecodeResult.fail(message, history)
        case Right((cmd, filename, args, time)) => {
          // Now check which of Remarks,Results key is present.
          val remarksDecode = (c --\ Remarks).as[List[String]].toOption
          val resultsDecode = (c --\ Results).as[List[Result]].toOption

          (remarksDecode, resultsDecode) match {
            case (Some(remarks), None) => {
              DecodeResult.ok(new TestCaseResult(cmd, filename, args, time, Left(remarks)))
            }
            case (_, Some(results)) => {
              DecodeResult.ok(new TestCaseResult(cmd, filename, args, time, Right(results)))
            }
            case (_, _) => {
              DecodeResult.fail("Didn't have remarks, results as expected", c.history)
            }
          }
        }
      }
    })
}

abstract class Json[T] {
  implicit def encode: EncodeJson[T]

  implicit def decode: DecodeJson[T]

  def dump(x: T)(): String =
    x.asJson.spaces2

  def load(j: String): Option[T] =
    j.decodeOption[T]
}

object ResultJson extends Json[Result] with ResultImplicits {
  implicit def encode = ResultEncodeJson
  implicit def decode = ResultDecodeJson
}

object TestCaseResultJson extends Json[TestCaseResult] with TestCaseResultImplicits {
  implicit def encode = TestCaseResultEncodeJson
  implicit def decode = TestCaseResultDecodeJson
}