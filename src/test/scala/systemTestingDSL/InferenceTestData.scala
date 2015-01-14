package systemTestingDSL

object InferenceTestData {
  val infer1Output: String = """
    Starting Omega...oc

Entail (1) : Valid.

 Residue:
    <1>true&q=null & n=inf_n_27 & inf_ann_26<=0&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_27>@inf_ann_26[Orig][LHSCase]]
 inferred pure: [inf_ann_26<=0]


Entail (2) : Valid.

 Residue:
    <1>true&q=null & n=0&{FLOW,(17,18)=__norm}[]
 inferred pure: [n=0]


Entail (3) : Valid.

 Residue:
    <1>true&q!=null & n=inf_n_42 & inf_ann_41<=0&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_42>@inf_ann_41[Orig][LHSCase]]
 inferred pure: [inf_ann_41<=0]


Entail (4) : Fail.(may) cause:15.4 no match for rhs data node: q (may-bug).

Residue:


Entail (5) : Valid.

 Residue:
    <1>true&n=0 & inf_ann_60<=0 & inf_n_61<=0&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_61>@inf_ann_60[Orig][LHSCase]]
 inferred pure: [inf_n_61<=0; inf_ann_60<=0]


Entail (6) : Valid.

 Residue:
    <1>false&false&{FLOW,(17,18)=__norm}[]
 inferred pure: [n!=0]


Entail (7) : Valid.

 Residue:
    <1>true&n=0 & inf_ann_85<=0 & n=inf_n_86&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_86>@inf_ann_85[Orig][LHSCase]]
 inferred pure: [n=inf_n_86; inf_ann_85<=0]


Entail (8) : Valid.

 Residue:
    <1>true&n=0 & inf_ann_99<=0 & n=inf_n_100&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_100>@inf_ann_99[Orig][LHSCase]]
 inferred pure: [n=inf_n_100; inf_ann_99<=0]


Entail (9) : Valid.

 Residue:
    <1>false&false&{FLOW,(17,18)=__norm}[]
 inferred pure: [n=0]

Stop Omega... 140 invocations"""

  val infer2Output: String = """
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


    Entail (3) : Valid.

	Residue:
    <1>true&q!=null & n=inf_n_42 & inf_ann_41<=0&{FLOW,(17,18)=__norm}[]
	inferred heap: [q::ll<inf_n_42>@inf_ann_41[Orig][LHSCase]]
	inferred pure: [inf_ann_41<=0]


    Entail (4) : Fail.(may) cause:15.4 no match for rhs data node: q (may-bug).

    Residue:


    Entail (5) : Valid.

Residue:
    <1>true&n=0 & inf_ann_60<=0 & inf_n_61<=0&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_61>@inf_ann_60[Orig][LHSCase]]
 inferred pure: [inf_n_61<=0; inf_ann_60<=0]


    Entail (6) : Valid.

 Residue:<1>false&false&{FLOW,(17,18)=__norm}[]
 inferred pure: [n!=0]


Entail (7) : Valid.

 Residue:<1>true&n=0 & inf_ann_85<=0 & n=inf_n_86&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_86>@inf_ann_85[Orig][LHSCase]]
 inferred pure: [n=inf_n_86; inf_ann_85<=0]


Entail (8) : Valid.

 Residue:<1>true&n=0 & inf_ann_99<=0 & n=inf_n_100&{FLOW,(17,18)=__norm}[]
 inferred heap: [q::ll<inf_n_100>@inf_ann_99[Orig][LHSCase]]
 inferred pure: [n=inf_n_100; inf_ann_99<=0]


Entail (9) : Valid.

 Residue:<1>false&false&{FLOW,(17,18)=__norm}[]
 inferred pure: [n=0]

Stop Omega... 140 invocations"""

  val infer4Output: String = """Starting Omega...oc

	Entail (1) : Fail.(may) cause: true |-  y!=null. LOCS:[11;0] (may-bug)

    Residue:

	Stop Omega... 16 invocations"""

}