package edu.nus.systemtesting.hipsleek

import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.output.ReporterColors._
import edu.nus.systemtesting.testsuite.TestSuiteResult
import GlobalReporter.reporter
import scala.util.matching.Regex

/**
 * Proprietary Sleek/Hip result analysis,
 * particularly for showing tally of reasons why
 * [[edu.nus.systemtesting.TestCase]] may not have returned a successful
 * execution.
 *
 * @author richardg
 */
class TestSuiteResultAnalysis(tsr: TestSuiteResult) {
  // TestCases is TestSuiteResult may fail for a number of reasons.
  // Scan output to see if it's a reason we're aware of.
  import tsr.invalid

  private val invalidCt = invalid.size
  private val IsOutputInvalidReasonTCs = true

  // return list of matched strings
  def outputMatchingRegex(tcr: TestCaseResult, regex: Regex): List[String] = {
    // Need to get lines of remark, since all output can be
    // as a single string.
    val remLines = (tcr.remarks toList) flatMap { rem => rem.lines.toList }

    remLines flatMap { line => (regex findFirstIn line) }
  }

  def testCasesWithOutputMatching(regex: Regex): List[TestCaseResult] = {
    invalid filterNot { tcr =>
      outputMatchingRegex(tcr, regex).isEmpty
    }
  }


  // Overspecified, Underspecified
  val badSpecTCs = testCasesWithOutputMatching("Underspecified!|Overspecified!".r)

  // Timeout
  val timeoutTCs = testCasesWithOutputMatching("TIMEOUT".r)

  // Barrrier b4n Fail
  val barrrierFailTCs = testCasesWithOutputMatching("Barrrier b4n Fail: .*".r)

  // redcsl (or other) not found
  val PNFRegex = "WARNING : Command for starting the prover \\((.*)\\) not found".r
  val proverNotFoundTCs = testCasesWithOutputMatching(PNFRegex)

  // exceptions
  // e.g.
  // Exception processed: Stream.Error("[id_ann_list_opt] expected after OBRACE (in [opt_brace_vars])")
  // Exception occurred: Parsing.Parse_error
  // Exception processed: Failure("\nInv Check: Fail.(View ll:Over)")
  // Exception occurred: Globals.Illegal_Prover_Format("z3.smt_of_typ: list(int) not supported for SMT")
  val ExcRegex = "Exception [^:]+: ([^\\(]*)(?:\\(\"(.*)\"\\))?".r
  val mappedInvalid =
    invalid flatMap { tcr =>
      val matching = outputMatchingRegex(tcr, ExcRegex)
      if (!matching.isEmpty)
        Some((tcr, matching))
      else
        None
    }

  val (exceptionTCs, allExcOutp) = mappedInvalid.unzip

  // Extract the kind of exception, and its (optional) reason
  val nameReasonPairs = (allExcOutp flatten) map { line =>
    line match {
      case ExcRegex(name, reason) =>
        (name, if (reason != null) Some(reason) else None)
    }
  }

  // Tally
  val excTally = (nameReasonPairs map { case (n, r) =>
    n + (r map { r => s"($r)"} getOrElse "")
  }).foldLeft(Map[String, Int]())({ (table, e) =>
    val nextCt = 1 + table.getOrElse(e, 0)
    table + (e -> nextCt)
  })

  // otherwise, unknown/unaccounted for...?
  val unaccounted =
    invalid diff
    (badSpecTCs union
     timeoutTCs union
     barrrierFailTCs union
     proverNotFoundTCs union
     exceptionTCs)


  def printTC(tcr: TestCaseResult): Unit = {
    import tcr.{ command, arguments, filename }
    reporter.println(s"$command $arguments $filename")
  }

  def outputReason(name: String, tcs: List[TestCaseResult]): Unit = {
    val TruncateTo = 5

    if (!tcs.isEmpty) {
      reporter.println(reporter.inColor(ColorWhite)(f"${name + ":"}%-30s ${s"(${tcs.size}/$invalidCt)"}%10s"))

      val iter = tcs.iterator
      val truncated = iter.take(TruncateTo).toList
      val remainingCt = iter.size

      if (IsOutputInvalidReasonTCs) {
        truncated.foreach(printTC)

        if (remainingCt > 0)
          reporter.println(remainingCt + " more...")
      }
    }
  }


  def report(): Unit = {
    reporter.println()
    reporter.println("Reasons for Invalid Test Cases:")

    outputReason("Bad Spec", badSpecTCs)
    outputReason("Timeout", timeoutTCs)
    outputReason("Barrrier Fail TCs", barrrierFailTCs)
    outputReason("Prover Not Found TCs", proverNotFoundTCs)
    outputReason("Exception Occurred TCs", exceptionTCs)

    // Turns out, there are many exceptions, but most occur only once
    val MinExcCount = 2
    val commonExcTally = excTally.filter({ case (e, ct) => ct >= MinExcCount })

    if (!commonExcTally.isEmpty) {
      println(s"Common Exception Reasons: (at least $MinExcCount occurrences)")
      commonExcTally.foreach({ case (e, ct) =>
        println(f"${e + ":"}%-50s $ct%4d")
      })
    }

    outputReason("Unaccounted For TCs", unaccounted)
    reporter.println()
  }
}



object TestSuiteResultAnalysis {
  def printTallyOfInvalidTests(tsr: TestSuiteResult): Unit = {
    val tsrAnalysis = new TestSuiteResultAnalysis(tsr)

    tsrAnalysis.report()
  }
}