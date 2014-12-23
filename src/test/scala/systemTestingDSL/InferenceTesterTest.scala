package systemTestingDSL

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.LinkedList

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
  def checkCorpusTest(): Unit = {
    new {
    } with InferenceTester {
      assertFalse(checkCorpus("""!!! Using Z3 by default
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



	Entail (3) : Valid.

	Residue:
	 <1>HP_62(d_61)&a=a_57 & b=b_58 & c=c_60 & d=d_61&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(y)&true --> y::node<a_57,b_58> * HP_59(b_58)&true(4,5);
	                  HP_59(b)&true --> b::node<c_60,d_61> * HP_62(d_61)&
	                   true(4,5)]
	[[ COND ==>  COND ==>  InferHeap ==>  COND ==>  InferHeap ==> ]]



	Entail (4) : Valid.

	Residue:
	 <1>emp&a=a_69 & b=b_70 & m=m_72&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(y)&true --> y::node<a_69,b_70> * HP_71(b_70)&true(4,5);
	                  HP_71(b)&true --> b::ll<m_72>&true(4,5)]
	[[ COND ==>  COND ==>  InferHeap ==>  COND ==>  InferHeap ==> ]]



	Entail (5) : Valid.

	Residue:
	 <1>emp&x=y & n=n_79&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(x)&true --> x::ll<n_79>&true(4,5)]
	[[ COND ==>  InferHeap ==> ]]



	Entail (6) : Valid.

	Residue:
	 <1>emp&n=0 & x=y&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(x)&n=0 --> x::ll<n>&true(4,5)]
	[[ COND ==>  InferHeap ==> ]]



	Entail (7) : Valid.

	Residue:
	 <1>emp&x=y & n=n_99&{FLOW,(4,5)=__norm#E}[]
	 inferred hprel: [H1(x)&true --> x::ll<n_99>&true(4,5)]
	[[ COND ==>  InferHeap ==> ]]



	Entail (8) : Valid.

	Residue:
	 <1>emp&a=ia & b=ib&{FLOW,(4,5)=__norm#E}[]
	[[ SEARCH ==>  Match(y,y) ==> ]]



	Entail (9) : Fail.(may) cause:AndR[ true |-  0<a_126. LOCS:[0;46];  true |-  flted_46_125=null. LOCS:[0;46] (may-bug).]

	Residue:
	 <1>HP_127(flted_46_125)&{FLOW,(4,8)=__MayError#E}[]
	 inferred hprel: [H1(y)&true --> y::node<a_126,flted_46_125> *
	                   HP_127(flted_46_125)&true(4,5)]
	[[ COND ==>  UnmatchedRHSData ==> ]]



	Entail (10) : Valid.

	Residue:
	 <1>emp&flted_50_131=null & 1<=aa&{FLOW,(4,5)=__norm#E}[]
	 inferred pure: [1<=aa]
	[[ SEARCH ==>  Match(y,y) ==> ]]



	Entail (11) : Valid.

	Residue:
	 <1>emp&n=m&{FLOW,(4,5)=__norm#E}[]
	[[ COND ==>  Match(y,y) ==> ]]



	Entail (12) : Valid.

	Residue:
	 <1>hfalse&false&{FLOW,(4,5)=__norm#E}[]


	Entail (13) : Valid.

	Residue:
	 <1>hfalse&false&{FLOW,(4,5)=__norm#E}[]

	Stop z3... 61 invocations Stop Omega... 40 invocations
	SAT Count   : 161
	SAT % Hit   : 76.39%
	IMPLY Count : 62
	IMPLY % Hit : 62.9%
	Time(cache overhead) : 0.012001 (seconds)

	!!! log(small):(0.089738,289)
	Total verification time: 0.108005 second(s)
		Time spent in main process: 0.060003 second(s)
		Time spent in child processes: 0.048002 second(s)""", new LinkedList[(String, String)]))
    }
  }
}