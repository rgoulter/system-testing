package edu.nus.systemtesting.hipsleek

import java.nio.file.Path
import java.nio.file.Paths
import edu.nus.systemtesting.ExecutionOutput
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.Result
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.TestCaseConfiguration
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.ProgramFlags.{ flagsOfProgram, isFlag }
import edu.nus.systemtesting.ExpectsOutput

object ValidateableSleekTestCase {
  implicit def constructTestCase(ps: PreparedSystem, tc: Testable with ExpectsOutput, conf: TestCaseConfiguration): ValidateableSleekTestCase =
    new ValidateableSleekTestCase(ps.binDir,
                                  tc.commandName,
                                  ps.corpusDir,
                                  tc.fileName,
                                  tc.arguments,
                                  tc.expectedOutput,
                                  conf.timeout)
}

class ValidateableSleekTestCase(binDir: Path = Paths.get(""),
                                cmd: Path = Paths.get(""),
                                corpDir: Path = Paths.get(""),
                                fn: Path = Paths.get(""),
                                args: String = "",
                                val expectedOutput: String = "",
                                timeout: Int = 300,
                                regex: String = "Validate \\d+: (OK|.*)")
    extends TestCase(binDir, cmd, corpDir, fn, args, timeout) with ExpectsOutput {

  // cf. sleekengine.ml process_validate function
  // regex to capture things like:
  //   Validate #: OK
  //   Validate #: Expecting flow $IDENT
  //   Validate #: Expecting$EXP but got no residue
  //   Validate #: Expecting$EXP but got : Unsat (or Unknown)
  //   Validate #: Expecting$EXP but got : Sat (or Unknown)
  //   Validate #: Expecting$EXP but got : $RES
  //   Validate #: Expecting(3)$EXP but got : $RES

  // Compared to typical SleekTestCase, the 'validation' is done by sleekengine.
  // (Rather than comparing against expectedOutput from some run-fast-tests list)

  // The above "Validate ..." needs to map to TestCaseResult which `checkResults`
  // was taking care of.
  //   OK         -> Result(idx, OK, OK)
  //   no residue -> Result(idx, OK, no residue)
  //   Unsat/Sat  -> Result(idx, OK, Unsat/Sat)
  //   $RES       -> Result(idx, OK, $EXP-but-got-$RES)

  def checkResults(output: ExecutionOutput): Either[List[String], Iterable[Result]] = {
    // `parse` is responsible for populating `results` with
    // lines which match `regex`.
    val results = filterLinesMatchingRegex(output.output, regex)

    if (results.isEmpty) {
      val testFlags = arguments.split(" ").filter(isFlag)
      val SleekFlags = flagsOfProgram(absCmdPath)
      val invalidFlags = testFlags.filterNot(SleekFlags.contains)

      if (!invalidFlags.isEmpty) {
        val flagsStr = invalidFlags.map(f => s"Invalid flag $f").mkString("\n")

        return Left(List("Binary failed to execute. Please investigate", flagsStr))
      } else {
        // Could try searching the output for errors?
        return Left(List("Binary failed to execute. Please investigate",
                         "Output was:") ++
                    output.stdoutLines ++
                    output.stderrLines)
      }
    }

    // Seems to be no impact of more `expect` commands than entailments.

    def resultFromOutputLine(resultLine: String): String = {
      // Always a `:` in the result Line.
      val validateResidue = resultLine.substring(resultLine.indexOf(':') + 2)

      val OkR = "OK".r
      val FlowR = "Expecting flow (.*)".r
      val NoResidueR = "Expecting(.*) BUT got no residue".r
      val ExpGotResidueR = "Expecting(.*) BUT got : (.*)".r

      // this crops up sometimes:
      val Exp3GotResidueR = "Expecting\\(3\\)(.*) but got : (.*)".r

      validateResidue match {
        case OkR() => "OK"
        case FlowR(ident) => validateResidue
        case NoResidueR(exp) => s"No residue (expecting $exp)"
        case ExpGotResidueR(exp,act) => s"Expecing $exp but got $act"
        case Exp3GotResidueR(exp,act) => s"Expecing $exp but got $act"
        case _ => s"UNKNOWN-VALIDATION: validateResidue"
      }
    }

    val resultUnits = results.zipWithIndex.flatMap({
      case (resultLine, idx) => {
        val expected = "OK"
        val actual = resultFromOutputLine(resultLine)

        Some(Result(idx.toString, expected, actual))
      }
    })

    return Right(resultUnits)
  }
}
