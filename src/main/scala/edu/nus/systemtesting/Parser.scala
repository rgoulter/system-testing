package edu.nus.systemtesting

/**
 * Used to filter a text to the lines which match a given regular expression.
 */
object Parser {
  /**
   * The matcher function splits a body of text using delimiters,
   * and returns an Array of lines which match the given regex.
   *
   * regex is matched using `Pattern.findFirstIn`.
   */
  def filterLinesMatchingRegex(text: String, regex: String) = {
    val lines = text.split("\n")

    lines.flatMap(line => {
      val pattern = regex.r
      pattern.findFirstIn(line)
    })
  }
}
