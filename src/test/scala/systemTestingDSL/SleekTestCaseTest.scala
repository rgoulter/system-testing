package test.scala.systemTestingDSL

import org.junit.Test
import org.junit.Assert._
import systemTestingDSL.Parser
import systemTestingDSL.SleekTestCase
import systemTestingDSL.SleekTestCaseBuilder
class SleekTestCaseTest {
  val lemmaOutput = """!!! init_tp : Using Z3 by default
Starting z3...
Starting Omega.../usr/local/bin/oc

Entailing lemma lseg: Valid.

Valid Lemmas : [lseg:==>] added to lemma store.

Entailing lemma tail2lsegL: Valid.

Valid Lemmas : [tail2lsegL:==>] added to lemma store.
Stop z3... 223 invocations Stop Omega... 48 invocations
SAT Count   : 673
SAT % Hit   : 76.82%
IMPLY Count : 100
IMPLY % Hit : 33.%
Time(cache overhead) : 0.040001 (seconds)

!!! log(small):(0.877273,891)
Total verification time: 0.472028 second(s)
	Time spent in main process: 0.244015 second(s)
	Time spent in child processes: 0.228013 second(s)
	"""
  @Test
  def splitOnRegexLemmasTest() = {

    var count = 0
    assertFalse(lemmaOutput.matches("Entail\\s\\d\\d:\\s.*Valid.*|Entail\\s\\d\\d:\\s.*Fail.*"))
    val splitResults = lemmaOutput.split("\n")
    for (line <- splitResults)
      if (line.stripMargin.matches("Entailing lemma.*:\\sValid.*|Entailing lemma.*:\\sFail.*"))
        count += 1
    assertEquals(2, count)
    assertTrue(lemmaOutput.contains("Entail"))
  }

  @Test
  def sleekTestCaseTest() = {
    val lemmasLsegTest = new SleekTestCase(
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/lseg.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_lseg" checkAgainst "Valid, Valid")
    lemmasLsegTest.parse(lemmaOutput, "Entailing lemma.*:\\sValid.*|Entailing lemma.*:\\sFail.*", "\n")
    val generatedResults = lemmasLsegTest.generateTestResult._1
    assertEquals(None, generatedResults)
  }

}