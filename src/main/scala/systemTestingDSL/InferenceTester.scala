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
    def matchResult: Boolean = entail.substring(entail.indexOf(":") + 1, entail.indexOf(".")).trim equals expectedResult.trim
    def matchResidue: Boolean = entail.substring(entail.indexOf("Residue:")).trim() equals expectedResidue.trim()
    if (validFormat)
      matchResult & matchResidue
    else validFormat
  }

  def checkCorpus(corpus: String, results: Seq[(String, String)]): Boolean = {
    def sanitizeCorpus: String = corpus.substring(0, corpus.indexOf("Stop")).substring(corpus.indexOf("Entail")).replace("\n\n", "\n").replace("\t", "").trim
    val splitCorpus: Array[String] = sanitizeCorpus.split("\n\n")
    println("Corpus length: " + splitCorpus.length)
    println("result length: " + results.length)
    println("Corpus")
    val zipped = splitCorpus.zipWithIndex
    //Broken need to fix
    // zipped.forall(case (index, tuple) => entailCheck(tuple._1, tuple._2 ))
    splitCorpus.length == results.length
  }
}