package test.scala.systemTestingDSL

object HipTestCaseData {

  val arr_sumOutput = """!!!Full processing file "hip/array/arr_sum.ss"
Parsing file "hip/array/arr_sum.ss" by default parser...
WARNING : parsing problem dom is neither a ranking function nor a relation nor a heap predicate
WARNING : parsing problem dom is neither a ranking function nor a relation nor a heap predicate

!!! processing primitives "["prelude.ss"]
Starting Omega.../usr/local/bin/oc

ERROR: at hip/array/arr_sum.ss_12:27_12:30
Message: TYPE ERROR 1 : Found NUM but expecting int
gather_type_info_b_formula: relation sumarray

Checking procedure sigmaleft$int[]~int~int... Omega Error Exp:Globals.Illegal_Prover_Format("Omega.omega_of_exp: array, bag or list constraint  a'[j']")
 Formula: !(v_bool_37_1000') & i'<=j' & j<=k & t<=i & dom(a,t,k) & j'=j & i'=i &
v_int_40_1078+1=j' & t=t_1065 & k=k_1066 & dom(a',t_1065,k_1066) &
t_1065<=i' & v_int_40_1078<=k_1066 &
sumarray(a',i',v_int_40_1078,v_int_40_998') & a=a' & t=ahalb & k=ahaub &
t_1065=ahalb & k_1066=ahaub & dom(a',ahalb,ahaub) & ahalb<=j' & j'<=ahaub &
res=a'[j']

!!! WARNING logtime exception:0.004
Procedure sigmaleft$int[]~int~int FAIL.(2)

Exception Globals.Illegal_Prover_Format("Omega.omega_of_exp: array, bag or list constraint  a'[j']") Occurred!

Error(s) detected when checking procedure sigmaleft$int[]~int~int
Omega Error Exp:Globals.Illegal_Prover_Format("Omega.omega_of_exp: array, bag or list constraint  a'[i']")
 Formula: !(v_bool_27_1032') & i'<=j' & dom(a,i,j) & i<=j & j'=j & i'=i & a=a' &
i=ahalb & j=ahaub & dom(a',ahalb,ahaub) & ahalb<=i' & i'<=ahaub & res=a'[i']

!!! WARNING logtime exception:0.
Checking procedure sigmaright$int[]~int~int... Omega Error Exp:Globals.Illegal_Prover_Format("Omega.omega_of_exp: array, bag or list constraint  a'[i']")
 Formula: !(v_bool_27_1032') & i'<=j' & dom(a,i,j) & i<=j & j'=j & i'=i & a=a' &
i=ahalb & j=ahaub & dom(a',ahalb,ahaub) & ahalb<=i' & i'<=ahaub & res=a'[i']

!!! WARNING logtime exception:0.
Procedure sigmaright$int[]~int~int FAIL.(2)

Exception Globals.Illegal_Prover_Format("Omega.omega_of_exp: array, bag or list constraint  a'[i']") Occurred!

Error(s) detected when checking procedure sigmaright$int[]~int~int

Checking procedure test$...
Procedure test$ SUCCESS.
Stop Omega... 62 invocations
2 false contexts at: ( (28,2)  (28,9) )

!!! log(small):(0.041829,215)
Total verification time: 0.136008 second(s)
	Time spent in main process: 0.100006 second(s)
	Time spent in child processes: 0.036002 second(s)

"""
}