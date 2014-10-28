package systemTestingDSL.matchers
import systemTestingDSL._

/**
 * Takes two files as data sources and performs a diff on them
 */
case class DiffMatcher(pathOne: String, pathTwo: String) extends Matcher {

  def matches(): Boolean = diff().length == 0

  def diff(): String = {
    val differ = new GeneralRunnable(commandName = "diff",
      fileName = pathOne,
      arguments = pathTwo)
    differ.execute
  }
}