package systemTestingDSL

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.LinkedList
import scala.collection.mutable.MutableList
import org.junit.Ignore
import scala.collection.mutable.ArrayBuffer

class InferenceTesterTest {

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
  def infer2Test(): Unit = {
    new {
    } with InferenceTester {
      assertFalse(checkCorpus(InferenceTestData.infer2Output, new MutableList[(String, String)]))
      val expectedResult = new MutableList[(String, String)]
      expectedResult += new Tuple2("Valid", """Residue:  <1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)]
	[[ COND ==>  InferHeap ==> ]]""")
      expectedResult += new Tuple2("Valid", """Residue:
	<1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[]
	inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)]
	    		  [[ COND ==>  InferHeap ==> ]]""")
      expectedResult += new Tuple2("Valid", """Residue: <1>true&q!=null & n=inf_n_42 & inf_ann_41<=0&{FLOW,(17,18)=__norm}[]
	 inferred heap: [q::ll<inf_n_42>@inf_ann_41[Orig][LHSCase]]
	 inferred pure: [inf_ann_41<=0]""")
      expectedResult += new Tuple2("Fail", """Residue:""")
      expectedResult += new Tuple2("Valid", """Residue: <1>true&n=0 & inf_ann_60<=0 & inf_n_61<=0&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_61>@inf_ann_60[Orig][LHSCase]]
 inferred pure: [inf_n_61<=0; inf_ann_60<=0]""")
      expectedResult += new Tuple2("Valid", """Residue: <1>false&false&{FLOW,(17,18)=__norm}[]
 inferred pure: [n!=0]""")
      expectedResult += new Tuple2("Valid", """Residue: <1>true&n=0 & inf_ann_85<=0 & n=inf_n_86&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_86>@inf_ann_85[Orig][LHSCase]]
 inferred pure: [n=inf_n_86; inf_ann_85<=0]""")
      expectedResult += new Tuple2("Valid", """Residue: <1>true&n=0 & inf_ann_99<=0 & n=inf_n_100&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_100>@inf_ann_99[Orig][LHSCase]]
 inferred pure: [n=inf_n_100; inf_ann_99<=0]""")
      expectedResult += new Tuple2("Valid", """Residue: <1>false&false&{FLOW,(17,18)=__norm}[]
 inferred pure: [n=0]""")
      assertTrue(checkCorpus(InferenceTestData.infer2Output, expectedResult))
    }
  }

  @Test
  def infer1Test(): Unit = {
    new {
    } with InferenceTester {
      val expectedResult = new ArrayBuffer[(String, String)]
      // Entail 1
      expectedResult += new Tuple2("Valid", """Residue: <1>true&q=null & n=inf_n_27 & inf_ann_26<=0&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_27>@inf_ann_26[Orig][LHSCase]]
 inferred pure: [inf_ann_26<=0]""")

      // Entail 2
      expectedResult += new Tuple2("Valid", """Residue:
    <1>true&q=null & n=0&{FLOW,(17,18)=__norm}[]
 inferred pure: [n=0]""")

      // Entail 3
      expectedResult += new Tuple2("Valid", """Residue:
    <1>true&q!=null & n=inf_n_42 & inf_ann_41<=0&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_42>@inf_ann_41[Orig][LHSCase]]
 inferred pure: [inf_ann_41<=0]""")

      // Entail 4
      expectedResult += new Tuple2("Fail", """Residue:""")

      // Entail 5
      expectedResult += new Tuple2("Valid", """Residue:
    <1>true&n=0 & inf_ann_60<=0 & inf_n_61<=0&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_61>@inf_ann_60[Orig][LHSCase]]
 inferred pure: [inf_n_61<=0; inf_ann_60<=0]""")

      // Entail 6
      expectedResult += new Tuple2("Valid", """Residue:
    <1>false&false&{FLOW,(17,18)=__norm}[]
 inferred pure: [n!=0]""")

      // Entail 7
      expectedResult += new Tuple2("Valid", """Residue:<1>true&n=0 & inf_ann_85<=0 & n=inf_n_86&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_86>@inf_ann_85[Orig][LHSCase]]
 inferred pure: [n=inf_n_86; inf_ann_85<=0]""")

      // Entail 8
      expectedResult += new Tuple2("Valid", """Residue:<1>true&n=0 & inf_ann_99<=0 & n=inf_n_100&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_100>@inf_ann_99[Orig][LHSCase]]
 inferred pure: [n=inf_n_100; inf_ann_99<=0]""")

      // Entail 9
      expectedResult += new Tuple2("Valid", """Residue:
    <1>false&false&{FLOW,(17,18)=__norm}[]
 inferred pure: [n=0]""")
      assertTrue(checkCorpus(InferenceTestData.infer1Output, expectedResult))
    }

  }
}