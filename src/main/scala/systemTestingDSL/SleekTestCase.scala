package systemTestingDSL

import scala.collection.mutable.MutableList

import systemTestingDSL.outputGenerator.ConsoleOutputGenerator

class SleekTestCaseBuilder() {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""
  var expectedOutput: String = ""
  var regex: String = "Entail.*:\\s.*Valid.*|Entail.*:\\s.*Fail.*|Entailing lemma.*:*Valid.*|Entailing lemma.*:.*Fail.*"

  def runCommand(commandName: String): SleekTestCaseBuilder = {
    this.commandName = commandName
    this
  }

  def onFile(fileName: String): SleekTestCaseBuilder = {
    this.fileName = fileName
    this
  }

  def withArguments(arguments: String): SleekTestCaseBuilder = {
    this.arguments = arguments
    this
  }

  def storeOutputInDirectory(outputDirectory: String): SleekTestCaseBuilder = {
    this.outputDirectory = outputDirectory
    this
  }

  def withOutputFileName(outputFileName: String): SleekTestCaseBuilder = {
    this.outputFileName = outputFileName
    this
  }

  def checkAgainst(expectedOutput: String): SleekTestCaseBuilder = {
    this.expectedOutput = expectedOutput
    this
  }

  def usingRegex(regex: String): SleekTestCaseBuilder = {
    this.regex = regex
    this
  }

  def build: SleekTestCase = new SleekTestCase(this)
}
class SleekTestCase(builder: SleekTestCaseBuilder)
    extends Runnable with Parser with InferenceTester with ConsoleOutputGenerator {
  var commandName = builder.commandName
  var fileName = builder.fileName
  var arguments = builder.arguments
  var outputFileName = builder.outputFileName
  var expectedOutput = builder.expectedOutput
  var outputDirectory = builder.outputDirectory
  var regex = builder.regex
  // (String,Long) tuple signifies that Runnable.execute returns (ConsoleOutput, TimeOfExecution)
  var output: (String, Long) = ("", 0)

  override def formCommand(): String = {
    commandName.concat(separator).concat(arguments).concat(separator).concat(fileName)
  }
  var results: MutableList[String] = MutableList()
  def process(source: String, rule: String): Unit = {
    results += rule
  }

  def run() = {
    this.output = this.execute
    if (outputFileName.length > 0)
      writeToFile(this.outputFileName, this.outputDirectory, output._1)
  }

  def generateOutput(): (Option[String], String, Long) = {
    run
    this.parse(this.output._1, builder.regex, NEW_LINE)
    generateTestResult

  }

  def checkResults(expectedOutput: String, result: Seq[String]): (Option[String], Boolean) = {
    val expectedOutputList: Array[String] = expectedOutput.split(DEFAULT_TEST_OUTPUT_SEPARATOR)
    var resultOutput = ""
    val filteredResults = results.view.filter(_.matches(builder.regex)).zipWithIndex
    if (filteredResults.isEmpty)
      return (Some("Binary failed to execute. Please investigate \n"), false)
    if (filteredResults.size != expectedOutputList.size)
      return matchUnequalFailedTests(results, expectedOutputList)
    for ((result, i) <- filteredResults)
      if (!result.contains(expectedOutputList(i))) {
        resultOutput += had(result)
        resultOutput += expected(expectedOutputList(i))
        return (Some(resultOutput), false)
      }
    return (None, true)
  }

  def matchUnequalFailedTests(filteredResults: Seq[String], expectedOutputList: Seq[String]): (Option[String], Boolean) = {
    val minSize = Math.min(filteredResults.length, expectedOutputList.size)
    var count, i = 0
    var unmatchedResults = ""
    for (count <- 0 until minSize) {
      if (!filteredResults(count).contains(expectedOutputList(count)))
        unmatchedResults += had(filteredResults(count))
      unmatchedResults += expected(expectedOutputList(count))
    }
    unmatchedResults += "\nUnmatched\n"
    unmatchedResults += "\nExtra Sleek Entail Output\n\n"
    for (i <- count until filteredResults.length)
      unmatchedResults += filteredResults(i)
    unmatchedResults += "\nExtra Results\n"
    for (i <- count until expectedOutputList.length)
      unmatchedResults += expectedOutputList(i)
    return (Some(unmatchedResults), false)
  }

  def generateTestResult(): (Option[String], String, Long) = {
    val results = checkResults(expectedOutput, this.results)
    if (results._2)
      (None, "Passed", output._2)
    else (results._1, "Failed", output._2)
  }
}
