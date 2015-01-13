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
    lazy val matchResult: Boolean = removeWhiteSpaceCharacters(entail.substring(entail.indexOf(":") + 1, entail.indexOf("."))) equals removeWhiteSpaceCharacters(expectedResult)
    lazy val matchResidue: Boolean = removeWhiteSpaceCharacters(entail.substring(entail.indexOf("Residue:"))) equals removeWhiteSpaceCharacters(expectedResidue)
    if (validFormat) {
//      println(matchResult)
//      println(matchResidue)
//      println("actual: " + removeWhiteSpaceCharacters(entail.substring(entail.indexOf("Residue:"))))
//      println("expected: " + removeWhiteSpaceCharacters(expectedResidue))
      matchResult & matchResidue
    } else validFormat
  }

  def removeWhiteSpaceCharacters(text: String): String = text.replace("\n", "").replace("\t", "").replace(" ", "").trim()

  def checkCorpus(corpus: String, results: Seq[(String, String)]): Boolean = {
    if (corpus.length() == 0 || results.length == 0)
      return false
    def sanitizeCorpus: String = corpus.substring(0, corpus.indexOf("Stop")).substring(corpus.indexOf("Entail")).replace("\n\n", "\n").replace("\t", "").trim
    val splitCorpus: Array[String] = sanitizeCorpus.split("\n\n")
//    println("Corpus length: " + splitCorpus.length)
//    println("result length: " + results.length)
    val zipped = splitCorpus.view.zipWithIndex
    zipped.forall(tuple => entailCheck(tuple._1, results(tuple._2)))
  }
}