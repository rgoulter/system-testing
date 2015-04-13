package systemTestingDSL

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import systemTestingDSL.outputGenerator.ConsoleOutputGenerator
import java.io.PrintWriter

case class SVCompTestSuite(directory: String,
    commandName: String = "hip",
    arguments: String = """-infer "@term" --svcomp-compete""",
    fileType: String = ".c",
    printer: PrintWriter = new PrintWriter(System.out, true)) extends GetFileList with ConsoleOutputGenerator {
  var tests = new HashMap[String, GenericTestCase]

  def buildResultMap(): HashMap[String, String] = {
    val files = getFileList(directory, fileType).filter(x => x.matches(".*true.*|.*false.*|.*unknown.*"))
    var resultMap = new HashMap[String, String]()
    files.foreach(file => resultMap.put(file, getResultFromFileName(extractFileNameFromPath(file))))
    files.foreach(file => tests.put(file, new GenericTestCase(commandName = this.commandName, arguments = this.arguments, fileName = file)))
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
      if (actualResult.equals(expectedResult._2))
        result += success("Passed")
      else {
        result += error("Failed")
        result += expected(expectedResult._2)
        result += actual(actualResult)
      }
    })
    printer.println(result)
  }

}
