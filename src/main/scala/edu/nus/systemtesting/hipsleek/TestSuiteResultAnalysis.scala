package edu.nus.systemtesting.hipsleek

import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.output.GlobalReporter
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
object TestSuiteResultAnalysis {
  def printTallyOfInvalidTests(tsr: TestSuiteResult): Unit = {
    // TestCases is TestSuiteResult may fail for a number of reasons.
    // Scan output to see if it's a reason we're aware of.
    import tsr.invalid

    val invalidCt = invalid.size
    val IsOutputInvalidReasonTCs = true

    def printTC(tcr: TestCaseResult): Unit = {
      import tcr.{ command, arguments, filename }
      println(s"$command $arguments $filename")
    }

    def outputReason(name: String, tcs: List[TestCaseResult]): Unit = {
      val TruncateTo = 5

      if (!tcs.isEmpty) {
        println(f"${name + ":"}%-30s ${s"(${tcs.size}/$invalidCt)"}%10s")

        val iter = tcs.iterator
        val truncated = iter.take(TruncateTo).toList
        val remainingCt = iter.size

        if (IsOutputInvalidReasonTCs) {
          truncated.foreach(printTC)

          if (remainingCt > 0)
            println(remainingCt + " more...")
        }
      }
    }

    // return list of matched strings
    def outputMatchingRegex(tcr: TestCaseResult, regex: Regex): List[String] = {
      // Need to get lines of remark, since all output can be
      // as a single string.
      val remLines = (tcr.remarks toList) map { rem => rem.lines.toList } flatten

      remLines map { line => (regex findFirstIn line) } flatten
    }

    def testCasesWithOutputMatching(regex: Regex): List[TestCaseResult] = {
      invalid filterNot { tcr =>
        outputMatchingRegex(tcr, regex).isEmpty
      }
    }


    // Overspecified, Underspecified
    val badSpecTCs = testCasesWithOutputMatching("Underspecified!|Overspecified!".r)

    outputReason("Bad Spec", badSpecTCs)

    // Timeout
    val timeoutTCs = testCasesWithOutputMatching("TIMEOUT".r)

    outputReason("Timeout", timeoutTCs)


    // Barrrier b4n Fail
    val barrrierFailTCs = testCasesWithOutputMatching("Barrrier b4n Fail: .*".r)

    outputReason("Barrrier Fail TCs", barrrierFailTCs)


    // redcsl (or other) not found
    val PNFRegex = "WARNING : Command for starting the prover \\((.*)\\) not found".r
    val proverNotFoundTCs = testCasesWithOutputMatching(PNFRegex)

    outputReason("Prover Not Found TCs", proverNotFoundTCs)


    // exceptions
    // e.g.
    // Exception processed: Stream.Error("[id_ann_list_opt] expected after OBRACE (in [opt_brace_vars])")
    // Exception occurred: Parsing.Parse_error
    // Exception processed: Failure("\nInv Check: Fail.(View ll:Over)")
    // Exception occurred: Globals.Illegal_Prover_Format("z3.smt_of_typ: list(int) not supported for SMT")
    val ExcRegex = "Exception [^:]+: ([^\\(]*)(?:\\(\"(.*)\"\\))?".r
    val mappedInvalid =
      invalid map { tcr =>
        val matching = outputMatchingRegex(tcr, ExcRegex)
        if (!matching.isEmpty)
          Some((tcr, matching))
        else
          None
      } flatten

    val (exceptionTCs, allExcOutp) = mappedInvalid.unzip

    outputReason("Exception Occurred TCs", exceptionTCs)

    // Extract the kind of exception, and its (optional) reason
    val nameReasonPairs = (allExcOutp flatten) map { line =>
      line match {
        case ExcRegex(name, reason) =>
          (name, if (reason != null) Some(reason) else None)
      }
    }

    // TODO: Tally
    println(nameReasonPairs)

    // otherwise, unknown/unaccounted for...?
    val unaccounted = invalid diff
                      (badSpecTCs union
                       timeoutTCs union
                       barrrierFailTCs union
                       proverNotFoundTCs union
                       exceptionTCs)

    outputReason("Unaccounted For TCs", unaccounted)
  }
}