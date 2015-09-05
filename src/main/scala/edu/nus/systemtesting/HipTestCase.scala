package edu.nus.systemtesting

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.output.ConsoleOutputGenerator

case class HipTestCase(commandName : String = "",
                       fileName : String = "",
                       arguments : String = "",
                       outputDirectory : String = "",
                       outputFileName : String = "",
                       expectedOutput : String = "",
                       regex : String = "Procedure.*FAIL.*|Procedure.*SUCCESS.*")
    extends Runnable with ConsoleOutputGenerator {
  override def formCommand() : String = {
    Seq(commandName, arguments, fileName).mkString(" ")
  }

  def buildExpectedOutputMap(results : String) : HashMap[String, String] = {
    // expected output is a string like "proc: SUCCESS, proc: FAIL"
    val outputMap = new HashMap[String, String]

    results.split(",").foreach(result =>
      outputMap.put(result.substring(0, result.indexOf(":")).trim,
                    result.substring(result.indexOf(":") + 1).trim))

    outputMap
  }

  def run() = {
    val res@(execOutp, time) = this.execute

    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectory, execOutp.output)

    res
  }

  def generateOutput() = {
    val (outp, time) = run

    generateTestResult(outp, time)
  }

  // TODO: Return type of Either would make more sense here?
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

  def generateTestResult(output : ExecutionOutput, time : Long) : TestCaseResult = {
    val (err, passed) = checkResults(expectedOutput, output)

    val result = if (passed) TestPassed else TestFailed

    new TestCaseResult(commandName, fileName, arguments, output, time, result, remarks = err.toList)
  }

  //
  // Helper functions for DSL-esque construction of testcase.
  //

  def runCommand(commandName : String) =
    copy(commandName = commandName)

  def onFile(fileName : String) =
    copy(fileName = fileName)

  def withArguments(arguments : String) =
    copy(arguments = arguments)

  def storeOutputInDirectory(outputDirectory : String) =
    copy(outputDirectory = outputDirectory)

  def withOutputFileName(outputFileName : String) =
    copy(outputFileName = outputFileName)

  def checkAgainst(expectedOutput : String) =
    copy(expectedOutput = expectedOutput)

  def usingRegex(regex : String) =
    copy(regex = regex)
}
