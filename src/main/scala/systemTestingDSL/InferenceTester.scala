package systemTestingDSL

trait InferenceTester {

  def fail: String = "Fail"

  def valid: String = "Valid"

  def isFail(result: String): Boolean = result.equals(fail)

  def isValid(result: String): Boolean = result.equals(valid)

  def entailCheck(entail: String, expectedResult: String, expectedResidue: String): Boolean = {
    def validFormat: Boolean = return entail.contains(":") && entail.contains(".") && entail.contains("Residue:")
    def matchResult: Boolean = return entail.substring(entail.indexOf(":") + 1, entail.indexOf(".")).trim equals expectedResult
    def matchResidue: Boolean = return entail.substring(entail.indexOf("Residue:")).trim() equals expectedResidue
    if (validFormat)
      matchResult & matchResidue
    else validFormat
  }

  def checkCorpus(corpus: String, results: List[(String, String)]) = {
    val splitCorpus = corpus.split("Entail\\s\\d\\d:\\s.*Valid.*|Entail\\s\\d\\d:\\s.*Fail.*\\n")
  }
}