package edu.nus.systemtesting

trait InferenceTester {
  def isFail(result: String): Boolean = result.equalsIgnoreCase(InferenceDefaults.FAIL)

  def isValid(result: String): Boolean = result.equalsIgnoreCase(InferenceDefaults.VALID)

  def entailCheck(entail: String, expected: (String, String)): Boolean = {
    //        println(entail)
    val (expectedResult, expectedResidue) = expected

    def validFormat: Boolean =
      entail.contains(InferenceDefaults.ENTAIL) && entail.contains(":") && entail.contains(".")

    val actualResult: String =
      InferenceDefaults.removeWhiteSpaceCharacters(entail.substring(entail.indexOf(":") + 1, entail.indexOf(".")))
    lazy val matchResult: Boolean =
      actualResult equals InferenceDefaults.removeWhiteSpaceCharacters(expectedResult)
    lazy val matchResidue: Boolean = {
      val v1 = InferenceDefaults.removeWhiteSpaceCharacters(entail.substring(entail.indexOf(InferenceDefaults.ANGLE_OPEN)))
      val v2 = InferenceDefaults.removeWhiteSpaceCharacters(expectedResidue)
      v1 equals v2
    }

    if (validFormat) {
      if (matchResult) {
        if (isFail(actualResult)) {
          //          println("Result: Fail")
          return true
        }

        if (isValid(actualResult)) {
          //          println("Result: Valid")
          //          println("actual: " + InferenceDefaults.removeWhiteSpaceCharacters(entail.substring(entail.indexOf(InferenceDefaults.ANGLE_OPEN))))
          //          println("Residue Match Result:" + matchResidue)
          //          println("expected: " + InferenceDefaults.removeWhiteSpaceCharacters(expectedResidue))
          return matchResidue
        }
      }

      return false
    } else {
      return validFormat
    }
  }

  def checkCorpus(corpus: String, results: Seq[(String, String)]): Boolean = {
    if (corpus.length() == 0 || results.length == 0)
      return false

    lazy val sanitizeCorpus: String = corpus.substring(0, corpus.indexOf(InferenceDefaults.STOP)).substring(corpus.indexOf(InferenceDefaults.ENTAIL)).replace(InferenceDefaults.DOUBLE_NEW_LINE, InferenceDefaults.NEW_LINE).replace("\t", "").trim
    //    println (sanitizeCorpus)

    val splitCorpus: Array[String] = sanitizeCorpus.split(InferenceDefaults.DOUBLE_NEW_LINE)
    //    println("Corpus length: " + splitCorpus.length)
    //    println("result length: " + results.length)

    val zipped = splitCorpus.view.zipWithIndex
    zipped.forall({ case (text, idx) => entailCheck(text, results(idx)) })
  }
}