package systemTestingDSL

import scala.sys.process._
import java.io._

trait Runnable {
  def commandName: String
  def currentWorkingDirectory: String
  def fileName: String
  def arguments: String
  def outputFile: String
  val separator: String = DEFAULT_SEPARATOR
  def execute: String = {
    val cmd = commandName.concat(separator).concat(arguments).concat(separator).concat(fileName)
    println(cmd)
    val result: String = cmd.!!
    if (outputFile.length() > 0)
      generateOutputFile(result)
    result
  }

  def generateOutputFile(consoleOutput: String) = {
    printToFile(new File(outputFile))(p => p.print(consoleOutput))
  }

}
