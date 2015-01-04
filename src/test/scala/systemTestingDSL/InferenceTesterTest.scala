package systemTestingDSL

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.LinkedList
import scala.collection.mutable.MutableList
import org.junit.Ignore

class InferenceTesterTest {

  val sampleCorpus: String = """
    !!! Using Z3 by default
	Starting z3...
	H1 is defined.
	H2 is defined.
	Starting Omega.../usr/local/bin/oc

	Entail (1) : Valid.

	Residue:
	 <1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)]
	[[ COND ==>  InferHeap ==> ]]



	Entail (2) : Valid.

	Residue:
	 <1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)]
	[[ COND ==>  InferHeap ==> ]]

    Stop z3... 61 invocations Stop Omega... 40 invocations
	SAT Count   : 161
	SAT % Hit   : 76.39%
	IMPLY Count : 62
	IMPLY % Hit : 62.9%
	Time(cache overhead) : 0.012001 (seconds)

	!!! log(small):(0.089738,289)
	Total verification time: 0.108005 second(s)
		Time spent in main process: 0.060003 second(s)
		Time spent in child processes: 0.048002 second(s)"""

  @Test
  def entailCheckTest(): Unit = {
    new {
    } with InferenceTester {
      assertTrue(entailCheck("""Entail 1: Valid.
          Residue: <1>hfalse&false&{FLOW,(4,5)=__norm#E}[] inferred pure: [x!=null]).(Fail,)."],""",
        ("Valid",
          """Residue: <1>hfalse&false&{FLOW,(4,5)=__norm#E}[] inferred pure: [x!=null]).(Fail,)."],""")))
      assertTrue(entailCheck("""Entail (1) : Valid.
    		Residue:
    		<1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)] [[ COND ==>  InferHeap ==> ]]""", ("Valid", """Residue:
    		<1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)] [[ COND ==>  InferHeap ==> ]]""")))
      assertFalse(entailCheck("""Entail (1) : Fail.
    		Residue:
    		<1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)] [[ COND ==>  InferHeap ==> ]]""",
        ("Valid", """Residue:
    		<1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)] [[ COND ==>  InferHeap ==> ]]""")))
      assertTrue(entailCheck("""Entail (2) : Valid. Residue: <1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)] [[ COND ==>  InferHeap ==> ]]""", ("Valid",
        """Residue: <1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)] [[ COND ==>  InferHeap ==> ]]""")))
      assertFalse(entailCheck("""Entail (2) : Fail. """, ("Valid",
        """Residue: <1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)] [[ COND ==>  InferHeap ==> ]]""")))
    }
  }

  @Test
  def checkCorpusTest(): Unit = {
    new {
    } with InferenceTester {
      assertFalse(checkCorpus(sampleCorpus, new MutableList[(String, String)]))
      val expectedResult = new MutableList[(String, String)]
      expectedResult += new Tuple2("Valid", """Residue:  <1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)]
	[[ COND ==>  InferHeap ==> ]]""")
      expectedResult += new Tuple2("Valid", """Residue:
	<1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[]
	inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)]
	    		  [[ COND ==>  InferHeap ==> ]]""")
      assertTrue(checkCorpus(sampleCorpus, expectedResult))

    }
  }
}