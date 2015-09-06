package edu.nus.systemtesting

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex

object HipTestCase {
  implicit def constructHipTestCase(tc : TestCaseBuilder) : HipTestCase =
    new HipTestCase(tc.commandName,
                    tc.fileName,
                    tc.arguments,
                    tc.outputDirectory,
                    tc.outputFileName,
                    tc.expectedOutput)
}

class HipTestCase(cmd : String = "",
                  fn : String = "",
                  args : String = "",
                  outDir : String = "",
                  outFN : String = "",
                  expectedOut : String = "",
                  regex : String = "Procedure.*FAIL.*|Procedure.*SUCCESS.*")
    extends TestCase(cmd, fn, args, outDir, outFN, expectedOut) {
  def buildExpectedOutputMap(results : String) : Map[String, String] = {
    // expected output is a string like "proc: SUCCESS, proc: FAIL"
    results.split(",").map(result =>
      (result.substring(0, result.indexOf(":")).trim,
       result.substring(result.indexOf(":") + 1).trim)).toMap
  }

  def checkResults(expectedOutput : String, output : ExecutionOutput) : (Option[String], Boolean) = {
    val expectedOutputMap = buildExpectedOutputMap(expectedOutput)

    // `parse` is responsible for populating `results` with
    // lines which match `builder.regex`.
    val results = filterLinesMatchingRegex(output.output, regex)
    val filteredResults = results.zipWithIndex

    var resultOutput = ""

    if (filteredResults.isEmpty)
      return (Some("Binary failed to execute. Please investigate \n"), false)

    for ((outputLine, idx) <- filteredResults) {
      var methodName = outputLine.split(" ")(1)
      methodName = methodName.substring(0, methodName.indexOf("$"))

      val result : String =
        if (outputLine.contains("FAIL"))
          "FAIL"
        else
          "SUCCESS"

      if (expectedOutputMap.contains(methodName) && !expectedOutputMap(methodName).equals(result)) {
        // TODO: get rid of `had`/`expected` here.
        resultOutput += had(result)
        resultOutput += expected(expectedOutputMap(methodName))
        return (Some(resultOutput), false)
      }
    }

    return (None, true)
  }
}
