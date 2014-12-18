package systemTestingDSL

import scala.collection.mutable.MutableList
import java.io.File

class SleekTestCaseBuilder {
  var commandName: String = ""
  var fileName: String = ""
  var arguments: String = ""
  var outputDirectory: String = ""
  var outputFileName: String = ""
  var expectedOutput: String = ""
  var regex: String = "Entail\\s\\d\\d:\\s.*Valid.*|Entail\\s\\d\\d:\\s.*Fail.*"

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
  extends Runnable with Parser {
  var commandName = builder.commandName
  var fileName = builder.fileName
  var arguments = builder.arguments
  var outputFileName = builder.outputFileName
  var expectedOutput = builder.expectedOutput
  var outputDirectory = builder.outputDirectory
  var regex = builder.regex

  var results: MutableList[String] = MutableList()
  def process(source: String, rule: String): Unit = {
    results += rule
  }

  def run() = {
    val output = this.execute
    if (outputFileName.length > 0)
      generateOutputFile(output)
    output
  }

  def printResults() = {
    for (result <- results)
      println(result + ", ")
    print("end of results")
  }

  def generateOutput() = {
    this.parse(run, builder.regex, NEW_LINE)
    generateTestResults()
  }

  def checkResults(): Boolean = {
    val expectedOutputList: Array[String] = expectedOutput.split(DEFAULT_TEST_OUTPUT_SEPARATOR)
println("results size: " + results.size)
println("expected results size: " + expectedOutput.size)
for((line) <- results)
      {
      println(line)
      println()
      }
for(line <- expectedOutputList)
  {println(line)
  println()
}
val filteredResults = results.view.filter(line => line.contains("Entail")).zipWithIndex
if (filteredResults.size != expectedOutputList.size)
return false
    for ((x, i) <- filteredResults)
      if (!x.contains(expectedOutputList(i)))
        {
          println(x)
        println(expectedOutputList(i))
          return false
        }
    return true
  }

  def generateTestResults(): String = if (checkResults()) "Passed" else "Failed"

  def checkOutputDirectory = {
    val outputDirectory = new File(this.outputDirectory)
    if (!outputDirectory.exists())
      fileSystemUtilities.createDirectory(this.outputDirectory)
  }

  def generateOutputFile(consoleOutput: String) = {
    checkOutputDirectory
    fileSystemUtilities.printToFile(new File(outputDirectory.concat(File.separator).concat(outputFileName)))(p => p.print(consoleOutput))
  }
}
