package edu.nus.systemtesting.matchers

import java.io.File

import scala.sys.process.Process

import edu.nus.systemtesting.FileSystemUtilities
import edu.nus.systemtesting.{ MATCHER_NEW, MATCHER_OLD, NEW_LINE, SPACE }

/**
 * Takes two files as data sources and performs a diff on them
 */
case class OutputToDiffMatcher(output: String, pathTwo: String) extends Matcher {
  def matches(): Boolean = diff().length == 0

  def diff(): String = {
    val tempFile = "/home/rohit/High-Performance-DSLs/temp"

    var resultsAfterReplacement: String = ""

    FileSystemUtilities.printToFile(new File(tempFile))(p => p.print(output))

    val diffProcName = "diff".concat(SPACE).concat("temp").concat(SPACE).concat(pathTwo)
    val results = Process(diffProcName).lines_!.foreach(line =>
      if (line.charAt(0) == '>') {
        resultsAfterReplacement += MATCHER_NEW.concat(line.substring(1)).concat(NEW_LINE)
      } else if (line.charAt(0) == '<') {
        resultsAfterReplacement += MATCHER_OLD.concat(line.substring(1)).concat(NEW_LINE)
      } else {
        resultsAfterReplacement += line.concat(NEW_LINE)
      })

    //val remove = Process("rm".concat(SPACE).concat("temp")).lines_!
    resultsAfterReplacement
  }
}
