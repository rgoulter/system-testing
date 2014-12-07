package systemTestingDSL.matchers
import java.io.File
import systemTestingDSL._
import scala.sys.process._

/**
 * Takes two files as data sources and performs a diff on them
 */
case class OutputToDiffMatcher(output: String, pathTwo: String) extends Matcher {

  def matches(): Boolean = diff().length == 0

  def diff(): String = {
    var resultsAfterReplacement: String = ""
    val tempFile = "/home/rohit/High-Performance-DSLs/temp"
    fileSystemUtilities.printToFile(new File(tempFile))(p => p.print(output))
    val results = Process("diff".concat(SPACE).concat("temp").concat(SPACE).concat(pathTwo)).lines_!.foreach(line =>
      if (line.charAt(0) == '>') {
        resultsAfterReplacement += MATCHER_NEW.concat(line.substring(1)).concat(NEW_LINE)
      } else if (line.charAt(0) == '<')
        resultsAfterReplacement += MATCHER_OLD.concat(line.substring(1)).concat(NEW_LINE)
      else
        resultsAfterReplacement += line.concat(NEW_LINE))
    val remove = Process("rm".concat(SPACE).concat("temp")).lines_!
    resultsAfterReplacement
  }
}
