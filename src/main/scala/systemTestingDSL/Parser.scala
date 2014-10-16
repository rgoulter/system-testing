package systemTestingDSL

trait Parser {

  // This should be overloaded
  def process(text: String, rule: String): Any

  /**
   * The matcher function splits a body of text using delimiters, finds matches and then processes them
   */
  def parse(text: String, rule: String, delimiter: String = DEFAULT_DELIMITER) = {
    val lines = text.split(delimiter)
    lines.foreach(perform(_, rule))
  }

  /**
   * If the string matches, it is processed
   */
  def perform(text: String, rule: String): Any = {
    val matched = matchString(text, rule)
    matched match {
      case Some(value) => process(text, value)
      case None =>
    }
  }

  /**
   * Performs a regex match on a string
   */
  def matchString(source: String, regex: String): Option[String] = {
    val pattern = regex.r
    pattern.findFirstIn(source)
  }
}