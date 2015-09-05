package edu.nus.systemtesting.matchers

import scala.sys.process.Process

import edu.nus.systemtesting.NEW_LINE
import edu.nus.systemtesting.SPACE

/**
 * Takes two files as data sources and performs a diff on them
 */
object DiffMatcher {
  def diff(pathOne : String, pathTwo : String) : String = {
    var resultsAfterReplacement = ""

    val procCmd = Seq("sdiff", pathOne, pathTwo).mkString(SPACE)
    val results = Process(procCmd).lines_!.foreach(line =>
      resultsAfterReplacement += line + NEW_LINE)

    resultsAfterReplacement
  }
}
