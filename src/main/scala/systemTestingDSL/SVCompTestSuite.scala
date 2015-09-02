package systemTestingDSL

import java.io.PrintWriter

import scala.collection.mutable.HashMap

import systemTestingDSL.outputGenerator.ConsoleOutputGenerator

case class SVCompTestSuite(directory: String,
                           commandName: String = "hip",
                           arguments: String = """-infer "@term" --svcomp-compete""",
                           fileType: String = ".c",
                           printer: PrintWriter = new PrintWriter(System.out, true))
    extends GetFileList with ConsoleOutputGenerator {
  var tests = new HashMap[String, SVCompTestCase]
  var failures: Int = 0
  var successes: Int = 0

  def buildResultMap(): HashMap[String, String] = {
    val files = getFileList(directory, fileType).filter(x => x.matches(".*true.*|.*false.*|.*unknown.*"))

    var resultMap = new HashMap[String, String]()

    files.foreach(file =>
      resultMap.put(file, getResultFromFileName(extractFileNameFromPath(file))))

    files.foreach(file =>
      tests.put(file, SVCompTestCase(commandName = this.commandName,
                                     arguments = this.arguments,
                                     fileName = file)))

    resultMap
  }

  def getResultFromFileName(fileName: String): String = {
    if (fileName.contains("true") || fileName.contains("TRUE"))
      return "TRUE"
    else if (fileName.contains("false") || fileName.contains("FALSE"))
      return "FALSE"

    "UNKNOWN"
  }

  def extractFileNameFromPath(path: String): String = {
    path.substring(path.lastIndexOf("/") + 1)
  }

  def runAllTests(): Unit = {
    var result = ""

    buildResultMap().foreach(expectedResult => {
      val actualResult = tests.get(expectedResult._1).get.runAndReturn

      result += log(expectedResult._1)

      if (actualResult.toLowerCase().contains(expectedResult._2.toLowerCase())) {
        result += success("Passed")
        successes += 1
      } else {
        result += error("Failed")
        result += expected(expectedResult._2)
        result += had(actualResult)
        failures += 1
      }
    })

    printer.println(result)
    printTestStatistics
  }

  def printTestStatistics(): Unit = {
    log("Total Number of tests: " + (successes + failures))
    success("Total Number of tests passed: " + successes)
    error("Total number of tests failed: " + failures)
  }
}
