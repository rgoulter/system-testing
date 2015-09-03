package edu.nus.systemtesting

object SleekTestCaseData {
  val veribsyncBarrierStaticOutput = """!! init_tp : Using Z3 by default
Starting Reduce...
Starting Omega.../usr/local/bin/oc
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 1: Valid.

Residue:
 <1>emp&bperm_6_331=0 & bperm_6_330=0 & p_329=Anon_11 & bperm_16_217=0 & bperm_16_218=2 & bperm_16_219=2 & bperm_16_218=t_328 & Anon_12=Anon_11 & Anon_13=p_329&{FLOW,(4,5)=__norm#E}[]
[[ COND ==>  SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> S-SPLIT(b,b)) ==>  COND ==>  Match(b,b) ==>  SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 2: Valid.

Residue:
 <1>emp&bperm_9_468=0 & bperm_9_469=c2_442+bperm_20_383 & p_447=p1_384 & bperm_9_448=bperm_20_378 & t_446=bperm_20_379 & c2_442=bperm_20_380 & p1_384=p1 & bperm_20_378=0 & bperm_20_379=2 & bperm_20_380=1 & bperm_20_381=0 & bperm_20_382=2 & bperm_20_383=1 & p2=p1&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)

Entail 3: Valid.

Residue:
 <1>htrue&p2_565=flted_24_533 & bperm_12_568=bperm_24_534 & t_567=bperm_24_535 & c2_563=bperm_24_536 & flted_24_533=1 & bperm_24_534=0 & bperm_24_535=2 & bperm_24_536=1 & flted_24_537=0 & bperm_24_538=0 & bperm_24_539=2 & bperm_24_540=1&{FLOW,(1,3)=__Fail#E}[]

Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 4: Valid.

Residue:
 <1>emp&bperm_9_700=0 & bperm_9_701=c2_674+bperm_28_601 & p_679=flted_28_594 & bperm_9_680=bperm_28_595 & t_678=bperm_28_596 & c2_674=bperm_28_597 & j=10+i & j'=10+j & i=0 & i'=10+i & flted_28_594+j=j' & bperm_28_595=0 & bperm_28_596=2 & bperm_28_597=1 & flted_28_598+i=i' & bperm_28_599=0 & bperm_28_600=2 & bperm_28_601=1 & Anon_14=flted_28_598&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)

Entail 5: Valid.

Residue:
 <1>emp&bperm_32_760=0 & bperm_32_761=2 & bperm_32_762=1&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Halting Reduce...
Stopping Reduce...
Stop Omega... 51 invocations
SAT Count   : 61
SAT % Hit   : 37.7%
IMPLY Count : 123
IMPLY % Hit : 61.78%
Time(cache overhead) : 0.008 (seconds)

!!! log(small):(0.140097,181)
!!!
 log(bigger)(>4s)(38):(570.,[(SAT:9<0:REDLOG (REDUCE LOGIC),15.);(SAT:18<1:REDLOG (REDUCE LOGIC),15.);(SAT:19<1:REDLOG (REDUCE LOGIC),15.);(SAT:30<6:REDLOG (REDUCE LOGIC),15.);(SAT:31<6:REDLOG (REDUCE LOGIC),15.);(SAT:32<6:REDLOG (REDUCE LOGIC),15.);(SAT:37<6:REDLOG (REDUCE LOGIC),15.);(SAT:38<6:REDLOG (REDUCE LOGIC),15.);(SAT:39<6:REDLOG (REDUCE LOGIC),15.);(SAT:41<6:REDLOG (REDUCE LOGIC),15.);(SAT:42<6:REDLOG (REDUCE LOGIC),15.);(SAT:43<6:REDLOG (REDUCE LOGIC),15.);(SAT:44<6:REDLOG (REDUCE LOGIC),15.);(SAT:56<7:REDLOG (REDUCE LOGIC),15.);(SAT:63<9:REDLOG (REDUCE LOGIC),15.);(SAT:64<9:REDLOG (REDUCE LOGIC),15.);(SAT:67<9:REDLOG (REDUCE LOGIC),15.);(SAT:68<9:REDLOG (REDUCE LOGIC),15.);(SAT:69<9:REDLOG (REDUCE LOGIC),15.);(SAT:70<9:REDLOG (REDUCE LOGIC),15.);(SAT:72<9:REDLOG (REDUCE LOGIC),15.);(SAT:73<9:REDLOG (REDUCE LOGIC),15.);(SAT:74<9:REDLOG (REDUCE LOGIC),15.);(SAT:79<11:REDLOG (REDUCE LOGIC),15.);(SAT:88<12:REDLOG (REDUCE LOGIC),15.);(SAT:97<13:REDLOG (REDUCE LOGIC),15.);(SAT:98<13:REDLOG (REDUCE LOGIC),15.);(SAT:105<15:REDLOG (REDUCE LOGIC),15.);(SAT:110<17:REDLOG (REDUCE LOGIC),15.);(SAT:111<17:REDLOG (REDUCE LOGIC),15.);(SAT:124<20:REDLOG (REDUCE LOGIC),15.);(SAT:135<22:REDLOG (REDUCE LOGIC),15.);(SAT:144<23:REDLOG (REDUCE LOGIC),15.);(SAT:153<24:REDLOG (REDUCE LOGIC),15.);(SAT:154<24:REDLOG (REDUCE LOGIC),15.);(SAT:161<26:REDLOG (REDUCE LOGIC),15.);(SAT:180<28:REDLOG (REDUCE LOGIC),15.);(SAT:188<30:REDLOG (REDUCE LOGIC),15.)])
Total verification time: 0.368022 second(s)
	Time spent in main process: 0.084005 second(s)
	Time spent in child processes: 0.284017 second(s)
"""

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
  val sleek3Output = """!!! init_tp : Using Z3 by default
Starting z3...
Starting Omega.../usr/local/bin/oc

Entailing lemma lem_12: Valid.

Valid Lemmas : [lem_12:==>] added to lemma store.

Entail 1: Valid.

Residue:
 <1>emp&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  (Lemma ==> lem_12(x,x)) ==> (left: lem_12) ==>  COND ==>  Match(x,x) ==> ]]



Entail 2: Fail.(must) cause: (((x=1 & 1<=10) | (x=null & 10=0))) & n=10 |-  11<n. LOCS:[1;3;0;17] (must-bug)


Entail 3: Valid.

Residue:
 <1>y::node<Anon_14,Anon_15>&0+1+1=c & r_187=r1 & Anon_186=Anon_170 & aa=null & 10<b & Anon_192=Anon_171 & r_193=aa&{FLOW,(4,5)=__norm#E}[]
[[ Fold ==>  SEARCH ==>  Match(x,x) ==>  Fold ==>  SEARCH ==>  Match(r1,r_187) ==>  COND ==>  BaseCaseFold ==> ]]


Stop z3... 66 invocations Stop Omega... 17 invocations
SAT Count   : 231
SAT % Hit   : 80.95%
IMPLY Count : 39
IMPLY % Hit : 43.58%
Time(cache overhead) : 0.004 (seconds)

!!! log(small):(0.109943,318)
Total verification time: 0.108005 second(s)
	Time spent in main process: 0.052003 second(s)
	Time spent in child processes: 0.056002 second(s)"""
}