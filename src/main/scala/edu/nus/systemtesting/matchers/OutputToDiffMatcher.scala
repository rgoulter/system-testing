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

    val diffProcName = Seq("diff", "temp", pathTwo).mkString(SPACE)
    val results = Process(diffProcName).lines_!.foreach(line =>
      if (line.charAt(0) == '>') {
        resultsAfterReplacement += MATCHER_NEW + line.substring(1) + NEW_LINE
      } else if (line.charAt(0) == '<') {
        resultsAfterReplacement += MATCHER_OLD + line.substring(1) + NEW_LINE
      } else {
        resultsAfterReplacement += line + NEW_LINE
      })

    //val remove = Process(Seq("rm", "temp").mkString(SPACE)).lines_!
    resultsAfterReplacement
  }
}
