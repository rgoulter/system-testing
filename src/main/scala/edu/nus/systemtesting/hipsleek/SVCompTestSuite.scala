package edu.nus.systemtesting.hipsleek

import java.io.PrintWriter
import edu.nus.systemtesting.FileSystemUtilities.getFileList
import edu.nus.systemtesting.output.ConsoleOutputGenerator

case class SVCompTestSuite(directory : String,
                           commandName : String = "hip",
                           arguments : String = """-infer "@term" --svcomp-compete""",
                           fileType : String = ".c",
                           printer : PrintWriter = new PrintWriter(System.out, true))
    extends ConsoleOutputGenerator {
  private val files = getFileList(directory, fileType).filter(x => x.matches(".*true.*|.*false.*|.*unknown.*"))

  val tests =
    files.map(file =>
      (file, SVCompTestCase(commandName = this.commandName,
                            arguments = this.arguments,
                            fileName = file))).toMap

  var failures = 0
  var successes = 0

  def buildResultMap() : Map[String, String] = {
    files.map(file =>
      (file, getResultFromFileName(extractFileNameFromPath(file)))).toMap
  }

  def getResultFromFileName(fileName : String) : String = {
    if (fileName.contains("true") || fileName.contains("TRUE"))
      return "TRUE"
    else if (fileName.contains("false") || fileName.contains("FALSE"))
      return "FALSE"

    "UNKNOWN"
  }

  // TODO: a.k.a. basename
  def extractFileNameFromPath(path : String) : String = {
    path.substring(path.lastIndexOf("/") + 1)
  }

  def runAllTests() : Unit = {
    var result = ""

    buildResultMap().foreach(expectedResult => {
      val (filename, expResult) = expectedResult
      val actualResult = tests.get(filename).get.runAndReturn

      result += log(filename)

      if (actualResult.toLowerCase().contains(expResult.toLowerCase())) {
        result += success("Passed")
        successes += 1
      } else {
        result += error("Failed")
        result += expected(expResult)
        result += had(actualResult)
        failures += 1
      }
    })

    printer.println(result)
    printTestStatistics
  }

  def printTestStatistics() : Unit = {
    log("Total Number of tests: " + (successes + failures))
    success("Total Number of tests passed: " + successes)
    error("Total number of tests failed: " + failures)
  }
}