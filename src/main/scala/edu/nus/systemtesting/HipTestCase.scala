package edu.nus.systemtesting

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

import edu.nus.systemtesting.Parser.filterLinesMatchingRegex
import edu.nus.systemtesting.output.ConsoleOutputGenerator

class HipTestCaseBuilder {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""
  var expectedOutput: String = ""
  var regex: String = "Procedure.*FAIL.*|Procedure.*SUCCESS.*"

  def runCommand(commandName: String): HipTestCaseBuilder = {
    this.commandName = commandName
    this
  }

  def onFile(fileName: String): HipTestCaseBuilder = {
    this.fileName = fileName
    this
  }

  def withArguments(arguments: String): HipTestCaseBuilder = {
    this.arguments = arguments
    this
  }

  def storeOutputInDirectory(outputDirectory: String): HipTestCaseBuilder = {
    this.outputDirectory = outputDirectory
    this
  }

  def withOutputFileName(outputFileName: String): HipTestCaseBuilder = {
    this.outputFileName = outputFileName
    this
  }

  def checkAgainst(expectedOutput: String): HipTestCaseBuilder = {
    this.expectedOutput = expectedOutput
    this
  }

  def usingRegex(regex: String): HipTestCaseBuilder = {
    this.regex = regex
    this
  }

  def build: HipTestCase = new HipTestCase(this)
}

class HipTestCase(builder: HipTestCaseBuilder)
    extends Runnable with ConsoleOutputGenerator {
  val commandName = builder.commandName
  val fileName = builder.fileName
  val arguments = builder.arguments
  val outputFileName = builder.outputFileName
  val expectedOutput = builder.expectedOutput
  val outputDirectory = builder.outputDirectory
  val regex = builder.regex

  override def formCommand(): String = {
    Seq(commandName, arguments, fileName).mkString(" ")
  }

  def buildExpectedOutputMap(results: String): HashMap[String, String] = {
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
  def checkResults(expectedOutput: String, output : ExecutionOutput): (Option[String], Boolean) = {
    val expectedOutputMap = buildExpectedOutputMap(output.output)

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

      val result: String =
        if (outputLine.contains("FAIL"))
          "FAIL"
        else
          "SUCCESS"

      if (expectedOutputMap.contains(methodName) && !expectedOutputMap(methodName).equals(result)) {
        resultOutput += had(result)
        resultOutput += expected(expectedOutputMap(methodName))
        return (Some(resultOutput), false)
      }
    }

    return (None, true)
  }

  def generateTestResult(output : ExecutionOutput, time : Long): (Option[String], String, Long) = {
    val (err, passed) = checkResults(expectedOutput, output)

    if (passed)
      (None, "Passed", time)
    else
      (err, "Failed", time)
  }
}