package systemTestingDSL

trait InferenceTester {

  def isFail(result: String): Boolean = result.equals(InferenceDefaults.FAIL)

  def isValid(result: String): Boolean = result.equals(InferenceDefaults.VALID)

  def entailCheck(entail: String, expected: (String, String)): Boolean = {
    val expectedResult: String = expected._1
    val expectedResidue: String = expected._2
    def validFormat: Boolean = entail.contains(InferenceDefaults.ENTAIL) && entail.contains(":") && entail.contains(".") && entail.contains(InferenceDefaults.RESIDUE)
    lazy val matchResult: Boolean = InferenceDefaults.removeWhiteSpaceCharacters(entail.substring(entail.indexOf(":") + 1, entail.indexOf("."))) equals InferenceDefaults.removeWhiteSpaceCharacters(expectedResult)
    lazy val matchResidue: Boolean = InferenceDefaults.removeWhiteSpaceCharacters(entail.substring(entail.indexOf(InferenceDefaults.RESIDUE))) equals InferenceDefaults.removeWhiteSpaceCharacters(expectedResidue)
    if (validFormat) {
      //      println(matchResult)
      //      println(matchResidue)
      //      println("actual: " + removeWhiteSpaceCharacters(entail.substring(entail.indexOf("Residue:"))))
      //      println("expected: " + removeWhiteSpaceCharacters(expectedResidue))
      matchResult & matchResidue
    } else validFormat
  }

  def checkCorpus(corpus: String, results: Seq[(String, String)]): Boolean = {
    if (corpus.length() == 0 || results.length == 0)
      return false
    lazy val sanitizeCorpus: String = corpus.substring(0, corpus.indexOf(InferenceDefaults.STOP)).substring(corpus.indexOf(InferenceDefaults.ENTAIL)).replace(InferenceDefaults.DOUBLE_NEW_LINE, InferenceDefaults.NEW_LINE).replace("\t", "").trim
    val splitCorpus: Array[String] = sanitizeCorpus.split(InferenceDefaults.DOUBLE_NEW_LINE)
    //    println("Corpus length: " + splitCorpus.length)
    //    println("result length: " + results.length)
    val zipped = splitCorpus.view.zipWithIndex
    zipped.forall(tuple => entailCheck(tuple._1, results(tuple._2)))
  }
}