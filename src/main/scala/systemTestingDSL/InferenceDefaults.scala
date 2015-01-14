package systemTestingDSL

object InferenceDefaults {

  val FAIL: String = "Fail"

  val VALID: String = "Valid"

  val RESIDUE: String = "Residue:"

  val STOP: String = "Stop"

  val ENTAIL: String = "Entail"

  def removeWhiteSpaceCharacters(text: String): String = text.replace("\n", "").replace("\t", "").replace(" ", "").trim()

  val NEW_LINE: String = "\n"

  val DOUBLE_NEW_LINE: String = NEW_LINE.concat(NEW_LINE)

  val TRIPLE_NEW_LINE: String = NEW_LINE.concat(NEW_LINE).concat(NEW_LINE)

  val ANGLE_OPEN: String = "<"

}