package edu.nus.systemtesting.hipsleek

object InferenceDefaults {
  val FAIL = "Fail"

  val VALID = "Valid"

  val RESIDUE = "Residue:"

  val STOP = "Stop"

  val ENTAIL = "Entail"

  def removeWhiteSpaceCharacters(text : String) : String =
    text.replace("\n", "").replace("\t", "").replace(" ", "").trim()

  val NEW_LINE = "\n"

  val DOUBLE_NEW_LINE = NEW_LINE * 2

  val TRIPLE_NEW_LINE = NEW_LINE * 3

  val ANGLE_OPEN = "<"
}
