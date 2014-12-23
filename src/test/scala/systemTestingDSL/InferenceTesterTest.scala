package systemTestingDSL

import org.junit.Test
import org.junit.Assert._

class InferenceTesterTest {

  @Test
  def entailCheckTestInfer2(): Unit = {
    new {
    } with InferenceTester {
      assertTrue(entailCheck("""Entail 1: Valid.
          Residue: <1>hfalse&false&{FLOW,(4,5)=__norm#E}[] inferred pure: [x!=null]).(Fail,)."],""",
        "Valid",
        """Residue: <1>hfalse&false&{FLOW,(4,5)=__norm#E}[] inferred pure: [x!=null]).(Fail,)."],"""))
      assertTrue(entailCheck("""Entail (1) : Valid.
    		Residue:
    		<1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)] [[ COND ==>  InferHeap ==> ]]""", "Valid", """Residue:
    		<1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)] [[ COND ==>  InferHeap ==> ]]"""))
      assertFalse(entailCheck("""Entail (1) : Fail.
    		Residue:
    		<1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)] [[ COND ==>  InferHeap ==> ]]""", "Valid", """Residue:
    		<1>HP_44(b_43)&a=a_42 & b=b_43&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::node<a_42,b_43> * HP_44(b_43)&true(4,5)] [[ COND ==>  InferHeap ==> ]]"""))
      assertTrue(entailCheck("""Entail (2) : Valid. Residue: <1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)] [[ COND ==>  InferHeap ==> ]]""", "Valid",
        """Residue: <1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)] [[ COND ==>  InferHeap ==> ]]"""))
      assertFalse(entailCheck("""Entail (2) : Fail. """, "Valid",
        """Residue: <1>emp&n=n_49&{FLOW,(4,5)=__norm#E}[] inferred hprel: [H1(y)&true --> y::ll<n_49>&true(4,5)] [[ COND ==>  InferHeap ==> ]]"""))
    }
  }
}