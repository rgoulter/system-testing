package systemTestingDSL

package object HTMLUtilities {
  def error(errorText: String) = paragraph(errorText, "red")

  def success(successText: String) = paragraph(successText, "green")

  def expected(expectedText: String) = paragraph(expectedText, "blue")

  def actual(actualText: String) = paragraph(actualText, "magenta")

  def log(logText: String) = paragraph(logText, "yellow")

  private def paragraph(content: String, colour: String = "black"): String = (s"""<p style = "color: $colour ;"> $content </p>""")

}