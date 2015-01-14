package systemTestingDSL

object InferenceDefaults {
  
  val FAIL: String = "Fail"

  val VALID: String = "Valid"

  val RESIDUE: String = "Residue:"

  val STOP: String = "Stop"

  val ENTAIL: String = "Entail"

  def removeWhiteSpaceCharacters(text: String): String = text.replace("\n", "").replace("\t", "").replace(" ", "").trim()

  def NEW_LINE: String = "\n"

  def DOUBLE_NEW_LINE: String = NEW_LINE.concat(NEW_LINE)

}