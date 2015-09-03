package edu.nus.systemtesting.output

trait HTMLOutputGenerator extends OutputGenerator {
  def error(errorText: String) = paragraph(errorText, "red")

  def success(successText: String) = paragraph(successText, "green")

  def expect(expectedText: String) = paragraph(expectedText, "blue")

  def actual(actualText: String) = paragraph(actualText, "magenta")

  def log(logText: String) = paragraph(logText, "rgb(207, 207, 58)")

  private def paragraph(content: String, colour: String = "black"): String =
    (s"""<p style = "color: $colour ;"> $content </p>""")
}