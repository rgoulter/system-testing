package systemTestingDSL.matchers
import systemTestingDSL._
import scala.sys.process._

/**
 * Takes two files as data sources and performs a diff on them
 */
case class DiffMatcher(pathOne: String, pathTwo: String) extends Matcher {

  def matches(): Boolean = diff().length == 0

  def diff(): String = {
    var resultsAfterReplacement: String = ""
    val results = Process("diff".concat(SPACE).concat(pathOne).concat(SPACE).concat(pathTwo)).lines_!.foreach(line =>
      if (line.charAt(0) == '>') {
        resultsAfterReplacement += MATCHER_NEW.concat(line.substring(1)).concat(NEW_LINE)
      } else if (line.charAt(0) == '<')
        resultsAfterReplacement += MATCHER_OLD.concat(line.substring(1)).concat(NEW_LINE)
      else
        resultsAfterReplacement += line.concat(NEW_LINE))
    resultsAfterReplacement
  }
}