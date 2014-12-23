package systemTestingDSL

trait InferenceTester {

  def fail: String = "Fail"

  def valid: String = "Valid"

  def isFail(result: String): Boolean = result.equals(fail)

  def isValid(result: String): Boolean = result.equals(valid)

  def entailCheck(entail: String, expected: (String, String)): Boolean = {
    val expectedResult: String = expected._1
    val expectedResidue: String = expected._2
    def validFormat: Boolean = entail.contains(":") && entail.contains(".") && entail.contains("Residue:")
    def matchResult: Boolean = entail.substring(entail.indexOf(":") + 1, entail.indexOf(".")).trim equals expectedResult
    def matchResidue: Boolean = entail.substring(entail.indexOf("Residue:")).trim() equals expectedResidue
    if (validFormat)
      matchResult & matchResidue
    else validFormat
  }

  def checkCorpus(corpus: String, results: Seq[(String, String)]): Boolean = {
    def sanitizeCorpus: String = corpus.substring(0, corpus.indexOf("Stop")).substring(corpus.indexOf("Entail"))
    val splitCorpus: Array[String] = sanitizeCorpus.split("Entail\\s\\d\\d:\\s.*Valid.*|Entail\\s\\d\\d:\\s.*Fail.*\\n")
    splitCorpus.length == results.length
  }
}