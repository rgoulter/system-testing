package test.scala.systemTestingDSL

import org.junit.Test
import org.junit.Assert._
import systemTestingDSL.Parser
import systemTestingDSL.SleekTestCase
import systemTestingDSL.SleekTestCaseBuilder
import org.junit.Ignore
class SleekTestCaseTest {

  val regex = "Entail.*:\\s.*Valid.*|Entail.*:\\s.*Fail.*|Entailing lemma.*:*Valid.*|Entailing lemma.*:.*Fail.*"

  val barrierDynamicOutput = """!!! init_tp : Using Z3 by default
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
Nested Timer(stop)
Nested Timer(stop)

Entail 1: Valid.

Residue:
 <1>emp&p_628=p & bperm_36_460=1 & bperm_36_461=3 & bperm_36_462=2 & bperm_36_461=t_627&{FLOW,(4,5)=__norm#E}[]
[[ COND ==>  SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-SPLIT(b,b)) ==>  COND ==>  Match(b,b) ==>  SEARCH ==>  Match(b,b) ==> ]]


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
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 2: Fail.(must) cause:12

Residue:
 <1>b::barrier( (bperm_40_733,bperm_40_732,bperm_40_731))<p>&bperm_40_731=1 & bperm_40_732=3 & bperm_40_733=1&{FLOW,(4,5)=__norm#E}[]
[[ COND ==>  SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> ]]


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

Entail 3: Valid.

Residue:
 <1>emp&bperm_45_1100=3 & bperm_45_1099=2 & bperm_45_1098=1 & bperm_10_1276=0 & bperm_10_1277=bperm_45_1098+bperm_45_1099&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> (left: D-FULL) ==>  SEARCH ==>  Match(b,b) ==> ]]

<2>emp&bperm_45_1100=3 & bperm_45_1099=2 & bperm_45_1098=1 & bperm_10_1302=0 & bperm_10_1303=bperm_45_1098+bperm_45_1099&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> (left: D-FULL) ==>  SEARCH ==>  Match(b,b) ==> ]]


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

Entail 4: Valid.

Residue:
 <1>emp&p_1678=p & bperm_49_1322=3 & bperm_49_1321=2 & bperm_49_1320=1 & bperm_10_1660=0 & bperm_10_1661=bperm_49_1320+bperm_49_1321 & bperm_10_1661=t_1677&{FLOW,(4,5)=__norm#E}[]
[[ COND ==>  SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> (left: D-FULL) ==>  COND ==>  SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-SPLIT(b,b)) ==>  COND ==>  Match(b,b) ==>  SEARCH ==>  Match(b,b) ==> ]]

<2>emp&p_1807=p & bperm_49_1322=3 & bperm_49_1321=2 & bperm_49_1320=1 & bperm_10_1789=0 & bperm_10_1790=bperm_49_1320+bperm_49_1321 & bperm_10_1790=t_1806&{FLOW,(4,5)=__norm#E}[]
[[ COND ==>  SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> (left: D-FULL) ==>  COND ==>  SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-SPLIT(b,b)) ==>  COND ==>  Match(b,b) ==>  SEARCH ==>  Match(b,b) ==> ]]


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

Entail 5: Valid.

Residue:
 <1>emp&bperm_13_2169=a2_2149+bperm_53_1916 & bperm_13_2170=c2_2150+bperm_53_1918 & p_2154=p & a2_2149=bperm_13_2073 & t_2153=bperm_53_1923 & c2_2150=bperm_13_2074 & bperm_13_2073=a2_2053+bperm_53_1922 & bperm_13_2074=c2_2054+bperm_53_1924 & p_2058=p_1925 & a2_2053=bperm_53_1919 & t_2057=bperm_53_1920 & c2_2054=bperm_53_1921 & bperm_53_1920=bperm_53_1917 & p_1926=p & p_1925=p & bperm_53_1916=9/5 & bperm_53_1917=3 & bperm_53_1918=3 & bperm_53_1919=6/5 & bperm_53_1920=3 & bperm_53_1921=2 & bperm_53_1922=0 & bperm_53_1923=3 & bperm_53_1924=1&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 6: Valid.

Residue:
 <1>emp&bperm_58_2363=1 & bperm_58_2362=3 & bperm_58_2361=0 & bperm_58_2360=2 & bperm_58_2359=3 & bperm_58_2358=6/5 & bperm_58_2357=3 & bperm_58_2356=3 & bperm_58_2355=9/5 & p_2364=p & p_2365=p & bperm_58_2359=bperm_58_2356 & c2_2493=bperm_58_2360 & t_2496=bperm_58_2359 & a2_2492=bperm_58_2358 & p_2497=p_2364 & bperm_13_2513=c2_2493+bperm_58_2363 & bperm_13_2512=a2_2492+bperm_58_2361 & c2_2589=bperm_13_2513 & t_2592=bperm_58_2362 & a2_2588=bperm_13_2512 & p_2593=p & bperm_13_2609=c2_2589+bperm_58_2357 & bperm_13_2608=a2_2588+bperm_58_2355 & bperm_10_2757=0 & bperm_10_2758=bperm_13_2608+bperm_58_2356&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> (left: D-FULL) ==>  SEARCH ==>  Match(b,b) ==> ]]

<2>emp&bperm_58_2363=1 & bperm_58_2362=3 & bperm_58_2361=0 & bperm_58_2360=2 & bperm_58_2359=3 & bperm_58_2358=6/5 & bperm_58_2357=3 & bperm_58_2356=3 & bperm_58_2355=9/5 & p_2364=p & p_2365=p & bperm_58_2359=bperm_58_2356 & c2_2493=bperm_58_2360 & t_2496=bperm_58_2359 & a2_2492=bperm_58_2358 & p_2497=p_2364 & bperm_13_2513=c2_2493+bperm_58_2363 & bperm_13_2512=a2_2492+bperm_58_2361 & c2_2589=bperm_13_2513 & t_2592=bperm_58_2362 & a2_2588=bperm_13_2512 & p_2593=p & bperm_13_2609=c2_2589+bperm_58_2357 & bperm_13_2608=a2_2588+bperm_58_2355 & bperm_10_2783=0 & bperm_10_2784=bperm_13_2608+bperm_58_2356&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> (left: D-FULL) ==>  SEARCH ==>  Match(b,b) ==> ]]


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
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 7: Valid.

Residue:
 <1>emp&p_2916=max(flted_62_2810,p2_2892) & bperm_19_2914=a2_2891+bperm_62_2811 & bperm_19_2915=0 & p2_2892=flted_62_2806 & a2_2891=bperm_62_2807 & t_2895=bperm_62_2808 & bperm_19_2896=bperm_62_2809 & flted_62_2806=2 & bperm_62_2807+1=0 & bperm_62_2808=3 & bperm_62_2809=0 & flted_62_2810=1 & bperm_62_2811+1=0 & bperm_62_2812=3 & bperm_62_2813=0&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


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

Entail 8: Valid.

Residue:
 <1>emp&p_3146=max(flted_66_3040,p2_3122) & bperm_19_3144=a2_3121+bperm_66_3041 & bperm_19_3145=0 & p2_3122=flted_66_3036 & a2_3121=bperm_66_3037 & t_3125=bperm_66_3038 & bperm_19_3126=bperm_66_3039 & flted_66_3036=1 & bperm_66_3037+1=0 & bperm_66_3038=3 & bperm_66_3039=0 & flted_66_3040=2 & bperm_66_3041+1=0 & bperm_66_3042=3 & bperm_66_3043=0&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 9: Fail.(must) cause: p_3376=max(2,1) |-  p_3376=1. LOCS:[0;19;70] (must-bug)

Residue:
 <1>emp&p_3376=max(flted_70_3270,p2_3352) & bperm_19_3374=a2_3351+bperm_70_3271 & bperm_19_3375=0 & p2_3352=flted_70_3266 & a2_3351=bperm_70_3267 & t_3355=bperm_70_3268 & bperm_19_3356=bperm_70_3269 & flted_70_3266=1 & bperm_70_3267+1=0 & bperm_70_3268=3 & bperm_70_3269=0 & flted_70_3270=2 & bperm_70_3271+1=0 & bperm_70_3272=3 & bperm_70_3273=0&{FLOW,(6,7)=__Error#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 10: Valid.

Residue:
 <1>emp&bperm_16_3620=a2_3603+bperm_74_3501 & p2_3605=flted_74_3496 & a2_3603=bperm_74_3497 & t_3607=bperm_74_3498 & c2_3604=bperm_74_3499 & flted_74_3496=1 & bperm_74_3497+1=0 & bperm_74_3498=3 & bperm_74_3499=0 & flted_74_3500=2 & bperm_74_3501=0 & bperm_74_3502=3 & bperm_74_3503=1&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


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

Entail 11: Valid.

Residue:
 <1>emp&bperm_16_3958=a2_3941+bperm_78_3741 & p2_3943=flted_78_3744 & a2_3941=bperm_78_3745 & t_3945=bperm_78_3746 & c2_3942=bperm_78_3747 & flted_78_3740=2 & bperm_78_3741=0 & bperm_78_3742=3 & bperm_78_3743=1 & flted_78_3744=1 & bperm_78_3745+1=0 & bperm_78_3746=3 & bperm_78_3747=0&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 12: Fail.(must) cause: flted_82_4078=2 |-  flted_82_4078=1. LOCS:[82] (must-bug)

Residue:
 <1>emp&bperm_16_4296=a2_4279+bperm_82_4079 & p2_4281=flted_82_4082 & a2_4279=bperm_82_4083 & t_4283=bperm_82_4084 & c2_4280=bperm_82_4085 & flted_82_4078=2 & bperm_82_4079=0 & bperm_82_4080=3 & bperm_82_4081=1 & flted_82_4082=1 & bperm_82_4083+1=0 & bperm_82_4084=3 & bperm_82_4085=0&{FLOW,(6,7)=__Error#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


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
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 13: Valid.

Residue:
 <1>emp&p_4642=p_4604 & p_4604=flted_86_4412 & flted_86_4412=0 & bperm_86_4413=0 & bperm_86_4414=3 & bperm_86_4415=3 & bperm_86_4414=t_4603 & t_4603=t_4641&{FLOW,(4,5)=__norm#E}[]
[[ COND ==>  SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-SPLIT(b,b)) ==>  COND ==>  Match(b,b) ==>  COND ==>  SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-SPLIT(b,b)) ==>  COND ==>  Match(b,b) ==>  SEARCH ==>  Match(b,b) ==> ]]


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
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 14: Valid.

Residue:
 <1>emp&bperm_16_5080=a2_5063+bperm_16_4911 & p2_5065=flted_90_4772 & a2_5063=bperm_90_4773 & t_5067=bperm_90_4774 & c2_5064=bperm_90_4775 & bperm_16_4911=a2_4894+bperm_90_4781 & p2_4896=flted_90_4776 & a2_4894=bperm_90_4777 & t_4898=bperm_90_4778 & c2_4895=bperm_90_4779 & bperm_90_4778=bperm_90_4774 & flted_90_4772=0 & bperm_90_4773+1=0 & bperm_90_4774=3 & bperm_90_4775=0 & flted_90_4776=1 & bperm_90_4777+1=0 & bperm_90_4778=3 & bperm_90_4779=0 & flted_90_4780=2 & bperm_90_4781=0 & bperm_90_4782=3 & bperm_90_4783=1&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 15: Valid.

Residue:
 <1>emp&bperm_94_5279=1 & bperm_94_5278=3 & bperm_94_5277=0 & flted_94_5276=2 & bperm_94_5275=0 & bperm_94_5274=3 & bperm_94_5273+1=0 & flted_94_5272=1 & bperm_94_5271=0 & bperm_94_5270=3 & bperm_94_5269+1=0 & flted_94_5268=0 & bperm_94_5274=bperm_94_5270 & c2_5390=bperm_94_5275 & t_5393=bperm_94_5274 & a2_5389=bperm_94_5273 & p2_5391=flted_94_5272 & bperm_16_5406=a2_5389+bperm_94_5277 & c2_5559=bperm_94_5271 & t_5562=bperm_94_5270 & a2_5558=bperm_94_5269 & p2_5560=flted_94_5268 & bperm_16_5575=a2_5558+bperm_16_5406 & bperm_10_5723=0 & bperm_10_5724=bperm_16_5575+bperm_94_5278&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==> (Match-->try split/combine) ==>  SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> (left: D-FULL) ==>  SEARCH ==>  Match(b,b) ==> ]]

<2>emp&bperm_94_5279=1 & bperm_94_5278=3 & bperm_94_5277=0 & flted_94_5276=2 & bperm_94_5275=0 & bperm_94_5274=3 & bperm_94_5273+1=0 & flted_94_5272=1 & bperm_94_5271=0 & bperm_94_5270=3 & bperm_94_5269+1=0 & flted_94_5268=0 & bperm_94_5274=bperm_94_5270 & c2_5390=bperm_94_5275 & t_5393=bperm_94_5274 & a2_5389=bperm_94_5273 & p2_5391=flted_94_5272 & bperm_16_5406=a2_5389+bperm_94_5277 & c2_5559=bperm_94_5271 & t_5562=bperm_94_5270 & a2_5558=bperm_94_5269 & p2_5560=flted_94_5268 & bperm_16_5575=a2_5558+bperm_16_5406 & bperm_10_5749=0 & bperm_10_5750=bperm_16_5575+bperm_94_5278&{FLOW,(4,5)=__norm#E}[]
[[ SEARCH ==>  (Lemma ==> D-FULL(b,b)) ==> (left: D-FULL) ==>  SEARCH ==>  Match(b,b) ==> ]]


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

Entail 16: Fail.(must) cause: flted_98_5768=2 |-  flted_98_5768=1. LOCS:[98] (must-bug)

Residue:
 <1>emp&flted_98_5768=2 & bperm_98_5769=0 & bperm_98_5770=2 & bperm_98_5771=1&{FLOW,(6,7)=__Error#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


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

Entail 17: Fail.(must) cause: flted_102_5917=2 |-  flted_102_5917=1. LOCS:[102] (must-bug)

Residue:
 <1>emp&flted_102_5917=2 & bperm_102_5918+1=0 & bperm_102_5919=2 & bperm_102_5920=0&{FLOW,(6,7)=__Error#E}[]
[[ SEARCH ==>  Match(b,b) ==> ]]


Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 18: Valid.

Residue:
 <1>htrue&p2_6121=flted_106_6072 & a2_6119=bperm_106_6073 & t_6123=bperm_106_6074 & c2_6120=bperm_106_6075 & flted_106_6072=2 & bperm_106_6073=0 & bperm_106_6074=3 & bperm_106_6075=1 & flted_106_6076=1 & bperm_106_6077=0 & bperm_106_6078=3 & bperm_106_6079=1&{FLOW,(1,3)=__Fail#E}[]

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
Nested Timer(stop)
Nested Timer(stop)

Entail 19: Valid.

Residue:
 <1>b::barrier( (bperm_111_6155,bperm_111_6154,bperm_111_6153))<flted_111_6152> * (htrue)&p2_6211=flted_111_6148 & a2_6209=bperm_111_6149 & t_6213=bperm_111_6150 & c2_6210=bperm_111_6151 & bperm_111_6154=bperm_111_6150 & flted_111_6148=2 & bperm_111_6149=0 & bperm_111_6150=3 & bperm_111_6151=1 & flted_111_6152=1 & bperm_111_6153=0 & bperm_111_6154=3 & bperm_111_6155=1 & flted_111_6156=1 & bperm_111_6157=0 & bperm_111_6158=3 & bperm_111_6159=1&{FLOW,(1,3)=__Fail#E}[]

Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 20: Valid.

Residue:
 <1>b::barrier( (bperm_115_6335,bperm_115_6334,bperm_115_6333))<flted_115_6332> * (htrue)&p2_6395=flted_115_6336 & a2_6393=bperm_115_6337 & t_6397=bperm_115_6338 & c2_6394=bperm_115_6339 & bperm_115_6338=bperm_115_6334 & flted_115_6332=1 & bperm_115_6333=0 & bperm_115_6334=3 & bperm_115_6335=1 & flted_115_6336=2 & bperm_115_6337=0 & bperm_115_6338=3 & bperm_115_6339=1 & flted_115_6340=1 & bperm_115_6341=0 & bperm_115_6342=3 & bperm_115_6343=1&{FLOW,(1,3)=__Fail#E}[]

Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 21: Fail.(must) cause:empty states

Residue:
 { }

Nested Timer(stop)
Nested Timer(stop)

Entail 22: Valid.

Residue:
 <1>htrue&p2_6773=flted_125_6740 & a2_6771=bperm_125_6741 & t_6775=bperm_125_6742 & c2_6772=bperm_125_6743 & flted_125_6740=2 & bperm_125_6741+1=0 & bperm_125_6742=3 & bperm_125_6743=0 & flted_125_6744=1 & bperm_125_6745=0 & bperm_125_6746=3 & bperm_125_6747=1&{FLOW,(1,3)=__Fail#E}[]

Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 23: Valid.

Residue:
 <1>htrue&p2_6923=flted_129_6800 & a2_6921=bperm_129_6801 & t_6925=bperm_129_6802 & c2_6922=bperm_129_6803 & flted_129_6796=1 & bperm_129_6797=0 & bperm_129_6798=3 & bperm_129_6799=1 & flted_129_6800=2 & bperm_129_6801+1=0 & bperm_129_6802=3 & bperm_129_6803=0&{FLOW,(1,3)=__Fail#E}[]

Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)
Nested Timer(stop)

Entail 24: Valid.

Residue:
 <1>b::barrier( (bperm_134_6957,bperm_134_6956,bperm_134_6955))<flted_134_6954> * (htrue)&p2_6997=flted_134_6950 & a2_6995=bperm_134_6951 & t_6999=bperm_134_6952 & c2_6996=bperm_134_6953 & bperm_134_6956=bperm_134_6952 & flted_134_6950=2 & bperm_134_6951+1=0 & bperm_134_6952=3 & bperm_134_6953=0 & flted_134_6954=1 & bperm_134_6955=0 & bperm_134_6956=3 & bperm_134_6957=1 & flted_134_6958=1 & bperm_134_6959=0 & bperm_134_6960=3 & bperm_134_6961=1&{FLOW,(1,3)=__Fail#E}[]

Nested Timer(stop)
Nested Timer(stop)

Entail 25: Valid.

Residue:
 <1>b::barrier( (bperm_138_7121,bperm_138_7120,bperm_138_7119))<flted_138_7118> * (htrue)&p2_7165=flted_138_7122 & a2_7163=bperm_138_7123 & t_7167=bperm_138_7124 & c2_7164=bperm_138_7125 & bperm_138_7124=bperm_138_7120 & flted_138_7118=1 & bperm_138_7119=0 & bperm_138_7120=3 & bperm_138_7121=1 & flted_138_7122=2 & bperm_138_7123+1=0 & bperm_138_7124=3 & bperm_138_7125=0 & flted_138_7126=1 & bperm_138_7127=0 & bperm_138_7128=3 & bperm_138_7129=1&{FLOW,(1,3)=__Fail#E}[]

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

Entail 26: Valid.

Residue:
 <1>b1::barrier( (c1,t1,a1))<p1> * b2::barrier( (c2,t2,a2))<p2>&t1!=t2 & b2!=b1&{FLOW,(4,5)=__norm#E}[]

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

Entail 27: Valid.

Residue:
 <1>b1::barrier( (c1,t1,a1))<p1> * b2::barrier( (c2,t2,a2))<p2>&(a2+t1+a1)<(c2+c1) & b2!=b1&{FLOW,(4,5)=__norm#E}[]

Halting Reduce...
Stopping Reduce...
Stop Omega... 314 invocations
SAT Count   : 1584
SAT % Hit   : 75.82%
IMPLY Count : 1874
IMPLY % Hit : 83.03%
Time(cache overhead) : 0.080008 (seconds)

!!! log(small):(2.143112,3546)
!!!
 log(bigger)(>4s)(411):(6165.,[(SAT:9<0:REDLOG (REDUCE LOGIC),15.);(SAT:18<1:REDLOG (REDUCE LOGIC),15.);(SAT:26<2:REDLOG (REDUCE LOGIC),15.);(SAT:35<3:REDLOG (REDUCE LOGIC),15.);(SAT:44<4:REDLOG (REDUCE LOGIC),15.);(SAT:45<4:REDLOG (REDUCE LOGIC),15.);(imply:48<6:REDLOG (REDUCE LOGIC),15.);(imply:50<7:REDLOG (REDUCE LOGIC),15.);(imply:52<8:REDLOG (REDUCE LOGIC),15.);(imply:55<9:REDLOG (REDUCE LOGIC),15.);(SAT:58<9:REDLOG (REDUCE LOGIC),15.);(SAT:59<9:REDLOG (REDUCE LOGIC),15.);(SAT:60<9:REDLOG (REDUCE LOGIC),15.);(SAT:62<9:REDLOG (REDUCE LOGIC),15.);(SAT:63<9:REDLOG (REDUCE LOGIC),15.);(SAT:64<9:REDLOG (REDUCE LOGIC),15.);(SAT:65<9:REDLOG (REDUCE LOGIC),15.);(SAT:66<9:REDLOG (REDUCE LOGIC),15.);(SAT:67<9:REDLOG (REDUCE LOGIC),15.);(SAT:68<9:REDLOG (REDUCE LOGIC),15.);(SAT:69<9:REDLOG (REDUCE LOGIC),15.);(SAT:70<9:REDLOG (REDUCE LOGIC),15.);(SAT:71<9:REDLOG (REDUCE LOGIC),15.);(SAT:72<9:REDLOG (REDUCE LOGIC),15.);(SAT:73<9:REDLOG (REDUCE LOGIC),15.);(SAT:74<9:REDLOG (REDUCE LOGIC),15.);(imply:82<10:REDLOG (REDUCE LOGIC),15.);(imply:84<10:REDLOG (REDUCE LOGIC),15.);(imply:85<10:REDLOG (REDUCE LOGIC),15.);(SAT:87<10:REDLOG (REDUCE LOGIC),15.);(SAT:116<13:REDLOG (REDUCE LOGIC),15.);(SAT:135<15:REDLOG (REDUCE LOGIC),15.);(imply:138<17:REDLOG (REDUCE LOGIC),15.);(imply:140<18:REDLOG (REDUCE LOGIC),15.);(imply:142<19:REDLOG (REDUCE LOGIC),15.);(SAT:149<20:REDLOG (REDUCE LOGIC),15.);(SAT:150<20:REDLOG (REDUCE LOGIC),15.);(SAT:151<20:REDLOG (REDUCE LOGIC),15.);(SAT:154<20:REDLOG (REDUCE LOGIC),15.);(SAT:157<20:REDLOG (REDUCE LOGIC),15.);(SAT:158<20:REDLOG (REDUCE LOGIC),15.);(SAT:159<20:REDLOG (REDUCE LOGIC),15.);(SAT:160<20:REDLOG (REDUCE LOGIC),15.);(SAT:161<20:REDLOG (REDUCE LOGIC),15.);(SAT:162<20:REDLOG (REDUCE LOGIC),15.);(SAT:163<20:REDLOG (REDUCE LOGIC),15.);(SAT:165<20:REDLOG (REDUCE LOGIC),15.);(SAT:199<21:REDLOG (REDUCE LOGIC),15.);(SAT:201<21:REDLOG (REDUCE LOGIC),15.);(SAT:204<21:REDLOG (REDUCE LOGIC),15.);(SAT:206<21:REDLOG (REDUCE LOGIC),15.);(SAT:207<21:REDLOG (REDUCE LOGIC),15.);(SAT:209<21:REDLOG (REDUCE LOGIC),15.);(SAT:210<21:REDLOG (REDUCE LOGIC),15.);(SAT:211<21:REDLOG (REDUCE LOGIC),15.);(SAT:212<21:REDLOG (REDUCE LOGIC),15.);(SAT:213<21:REDLOG (REDUCE LOGIC),15.);(SAT:214<21:REDLOG (REDUCE LOGIC),15.);(SAT:215<21:REDLOG (REDUCE LOGIC),15.);(SAT:216<21:REDLOG (REDUCE LOGIC),15.);(SAT:218<21:REDLOG (REDUCE LOGIC),15.);(SAT:219<21:REDLOG (REDUCE LOGIC),15.);(SAT:220<21:REDLOG (REDUCE LOGIC),15.);(imply:227<23:REDLOG (REDUCE LOGIC),15.);(SAT:234<24:REDLOG (REDUCE LOGIC),15.);(SAT:236<24:REDLOG (REDUCE LOGIC),15.);(SAT:238<24:REDLOG (REDUCE LOGIC),15.);(SAT:239<24:REDLOG (REDUCE LOGIC),15.);(SAT:240<24:REDLOG (REDUCE LOGIC),15.);(SAT:241<24:REDLOG (REDUCE LOGIC),15.);(SAT:242<24:REDLOG (REDUCE LOGIC),15.);(SAT:247<24:REDLOG (REDUCE LOGIC),15.);(SAT:282<25:REDLOG (REDUCE LOGIC),15.);(SAT:287<25:REDLOG (REDUCE LOGIC),15.);(SAT:290<25:REDLOG (REDUCE LOGIC),15.);(SAT:293<25:REDLOG (REDUCE LOGIC),15.);(SAT:294<25:REDLOG (REDUCE LOGIC),15.);(SAT:318<26:REDLOG (REDUCE LOGIC),15.);(SAT:335<27:REDLOG (REDUCE LOGIC),15.);(SAT:345<28:REDLOG (REDUCE LOGIC),15.);(SAT:354<29:REDLOG (REDUCE LOGIC),15.);(SAT:362<30:REDLOG (REDUCE LOGIC),15.);(SAT:371<31:REDLOG (REDUCE LOGIC),15.);(SAT:380<32:REDLOG (REDUCE LOGIC),15.);(SAT:381<32:REDLOG (REDUCE LOGIC),15.);(imply:385<34:REDLOG (REDUCE LOGIC),15.);(SAT:391<35:REDLOG (REDUCE LOGIC),15.);(SAT:392<35:REDLOG (REDUCE LOGIC),15.);(SAT:393<35:REDLOG (REDUCE LOGIC),15.);(SAT:396<35:REDLOG (REDUCE LOGIC),15.);(SAT:399<35:REDLOG (REDUCE LOGIC),15.);(SAT:407<37:REDLOG (REDUCE LOGIC),15.);(imply:469<47:REDLOG (REDUCE LOGIC),15.);(imply:471<48:REDLOG (REDUCE LOGIC),15.);(SAT:477<49:REDLOG (REDUCE LOGIC),15.);(SAT:478<49:REDLOG (REDUCE LOGIC),15.);(SAT:484<49:REDLOG (REDUCE LOGIC),15.);(SAT:500<50:REDLOG (REDUCE LOGIC),15.);(SAT:505<50:REDLOG (REDUCE LOGIC),15.);(SAT:507<50:REDLOG (REDUCE LOGIC),15.);(SAT:509<50:REDLOG (REDUCE LOGIC),15.);(SAT:510<50:REDLOG (REDUCE LOGIC),15.);(SAT:511<50:REDLOG (REDUCE LOGIC),15.);(SAT:512<50:REDLOG (REDUCE LOGIC),15.);(SAT:513<50:REDLOG (REDUCE LOGIC),15.);(SAT:514<50:REDLOG (REDUCE LOGIC),15.);(SAT:515<50:REDLOG (REDUCE LOGIC),15.);(SAT:516<50:REDLOG (REDUCE LOGIC),15.);(SAT:517<50:REDLOG (REDUCE LOGIC),15.);(imply:529<52:REDLOG (REDUCE LOGIC),15.);(SAT:536<53:REDLOG (REDUCE LOGIC),15.);(SAT:558<54:REDLOG (REDUCE LOGIC),15.);(SAT:563<54:REDLOG (REDUCE LOGIC),15.);(SAT:569<54:REDLOG (REDUCE LOGIC),15.);(imply:586<56:REDLOG (REDUCE LOGIC),15.);(imply:588<57:REDLOG (REDUCE LOGIC),15.);(imply:590<58:REDLOG (REDUCE LOGIC),15.);(SAT:622<60:REDLOG (REDUCE LOGIC),15.);(SAT:674<68:REDLOG (REDUCE LOGIC),15.);(SAT:675<68:REDLOG (REDUCE LOGIC),15.);(SAT:676<68:REDLOG (REDUCE LOGIC),15.);(SAT:677<68:REDLOG (REDUCE LOGIC),15.);(SAT:678<68:REDLOG (REDUCE LOGIC),15.);(SAT:679<68:REDLOG (REDUCE LOGIC),15.);(SAT:680<68:REDLOG (REDUCE LOGIC),15.);(SAT:681<68:REDLOG (REDUCE LOGIC),15.);(SAT:684<68:REDLOG (REDUCE LOGIC),15.);(SAT:685<68:REDLOG (REDUCE LOGIC),15.);(SAT:694<69:REDLOG (REDUCE LOGIC),15.);(SAT:695<69:REDLOG (REDUCE LOGIC),15.);(SAT:696<69:REDLOG (REDUCE LOGIC),15.);(SAT:700<69:REDLOG (REDUCE LOGIC),15.);(SAT:709<71:REDLOG (REDUCE LOGIC),15.);(SAT:713<71:REDLOG (REDUCE LOGIC),15.);(SAT:714<71:REDLOG (REDUCE LOGIC),15.);(SAT:731<72:REDLOG (REDUCE LOGIC),15.);(SAT:732<72:REDLOG (REDUCE LOGIC),15.);(SAT:765<77:REDLOG (REDUCE LOGIC),15.);(SAT:769<77:REDLOG (REDUCE LOGIC),15.);(SAT:774<77:REDLOG (REDUCE LOGIC),15.);(SAT:785<78:REDLOG (REDUCE LOGIC),15.);(SAT:790<78:REDLOG (REDUCE LOGIC),15.);(SAT:796<80:REDLOG (REDUCE LOGIC),15.);(SAT:805<82:REDLOG (REDUCE LOGIC),15.);(SAT:806<82:REDLOG (REDUCE LOGIC),15.);(SAT:807<82:REDLOG (REDUCE LOGIC),15.);(SAT:808<82:REDLOG (REDUCE LOGIC),15.);(SAT:811<82:REDLOG (REDUCE LOGIC),15.);(SAT:812<82:REDLOG (REDUCE LOGIC),15.);(SAT:824<84:REDLOG (REDUCE LOGIC),15.);(SAT:825<84:REDLOG (REDUCE LOGIC),15.);(SAT:851<88:REDLOG (REDUCE LOGIC),15.);(SAT:856<88:REDLOG (REDUCE LOGIC),15.);(SAT:862<90:REDLOG (REDUCE LOGIC),15.);(SAT:871<91:REDLOG (REDUCE LOGIC),15.);(SAT:880<92:REDLOG (REDUCE LOGIC),15.);(SAT:888<93:REDLOG (REDUCE LOGIC),15.);(SAT:897<94:REDLOG (REDUCE LOGIC),15.);(SAT:906<95:REDLOG (REDUCE LOGIC),15.);(SAT:907<95:REDLOG (REDUCE LOGIC),15.);(SAT:915<97:REDLOG (REDUCE LOGIC),15.);(SAT:917<97:REDLOG (REDUCE LOGIC),15.);(imply:921<99:REDLOG (REDUCE LOGIC),15.);(SAT:927<100:REDLOG (REDUCE LOGIC),15.);(SAT:928<100:REDLOG (REDUCE LOGIC),15.);(SAT:931<100:REDLOG (REDUCE LOGIC),15.);(SAT:932<100:REDLOG (REDUCE LOGIC),15.);(SAT:933<100:REDLOG (REDUCE LOGIC),15.);(SAT:942<101:REDLOG (REDUCE LOGIC),15.);(SAT:943<101:REDLOG (REDUCE LOGIC),15.);(SAT:944<101:REDLOG (REDUCE LOGIC),15.);(SAT:945<101:REDLOG (REDUCE LOGIC),15.);(SAT:946<101:REDLOG (REDUCE LOGIC),15.);(SAT:949<101:REDLOG (REDUCE LOGIC),15.);(SAT:950<101:REDLOG (REDUCE LOGIC),15.);(imply:1193<132:REDLOG (REDUCE LOGIC),15.);(SAT:1200<133:REDLOG (REDUCE LOGIC),15.);(SAT:1203<133:REDLOG (REDUCE LOGIC),15.);(SAT:1205<133:REDLOG (REDUCE LOGIC),15.);(SAT:1215<135:REDLOG (REDUCE LOGIC),15.);(SAT:1234<139:REDLOG (REDUCE LOGIC),15.);(SAT:1235<139:REDLOG (REDUCE LOGIC),15.);(SAT:1236<139:REDLOG (REDUCE LOGIC),15.);(SAT:1237<139:REDLOG (REDUCE LOGIC),15.);(SAT:1239<139:REDLOG (REDUCE LOGIC),15.);(SAT:1240<139:REDLOG (REDUCE LOGIC),15.);(SAT:1241<139:REDLOG (REDUCE LOGIC),15.);(SAT:1249<141:REDLOG (REDUCE LOGIC),15.);(SAT:1255<141:REDLOG (REDUCE LOGIC),15.);(SAT:1259<143:REDLOG (REDUCE LOGIC),15.);(SAT:1268<144:REDLOG (REDUCE LOGIC),15.);(SAT:1277<145:REDLOG (REDUCE LOGIC),15.);(SAT:1285<146:REDLOG (REDUCE LOGIC),15.);(SAT:1294<147:REDLOG (REDUCE LOGIC),15.);(SAT:1303<148:REDLOG (REDUCE LOGIC),15.);(SAT:1304<148:REDLOG (REDUCE LOGIC),15.);(SAT:1312<150:REDLOG (REDUCE LOGIC),15.);(SAT:1321<152:REDLOG (REDUCE LOGIC),15.);(SAT:1335<154:REDLOG (REDUCE LOGIC),15.);(SAT:1341<154:REDLOG (REDUCE LOGIC),15.);(SAT:1345<156:REDLOG (REDUCE LOGIC),15.);(SAT:1354<157:REDLOG (REDUCE LOGIC),15.);(SAT:1363<158:REDLOG (REDUCE LOGIC),15.);(SAT:1380<160:REDLOG (REDUCE LOGIC),15.);(SAT:1389<161:REDLOG (REDUCE LOGIC),15.);(SAT:1390<161:REDLOG (REDUCE LOGIC),15.);(SAT:1398<163:REDLOG (REDUCE LOGIC),15.);(SAT:1488<176:REDLOG (REDUCE LOGIC),15.);(SAT:1489<176:REDLOG (REDUCE LOGIC),15.);(SAT:1492<176:REDLOG (REDUCE LOGIC),15.);(SAT:1493<176:REDLOG (REDUCE LOGIC),15.);(SAT:1494<176:REDLOG (REDUCE LOGIC),15.);(SAT:1495<176:REDLOG (REDUCE LOGIC),15.);(SAT:1497<176:REDLOG (REDUCE LOGIC),15.);(SAT:1498<176:REDLOG (REDUCE LOGIC),15.);(SAT:1499<176:REDLOG (REDUCE LOGIC),15.);(SAT:1511<178:REDLOG (REDUCE LOGIC),15.);(SAT:1516<178:REDLOG (REDUCE LOGIC),15.);(SAT:1550<184:REDLOG (REDUCE LOGIC),15.);(SAT:1559<185:REDLOG (REDUCE LOGIC),15.);(SAT:1568<186:REDLOG (REDUCE LOGIC),15.);(SAT:1585<188:REDLOG (REDUCE LOGIC),15.);(SAT:1594<189:REDLOG (REDUCE LOGIC),15.);(SAT:1595<189:REDLOG (REDUCE LOGIC),15.);(SAT:1603<191:REDLOG (REDUCE LOGIC),15.);(SAT:1611<193:REDLOG (REDUCE LOGIC),15.);(SAT:1613<193:REDLOG (REDUCE LOGIC),15.);(SAT:1617<193:REDLOG (REDUCE LOGIC),15.);(SAT:1627<195:REDLOG (REDUCE LOGIC),15.);(SAT:1642<197:REDLOG (REDUCE LOGIC),15.);(SAT:1653<199:REDLOG (REDUCE LOGIC),15.);(SAT:1666<201:REDLOG (REDUCE LOGIC),15.);(SAT:1684<203:REDLOG (REDUCE LOGIC),15.);(SAT:1689<203:REDLOG (REDUCE LOGIC),15.);(SAT:1723<209:REDLOG (REDUCE LOGIC),15.);(SAT:1768<214:REDLOG (REDUCE LOGIC),15.);(SAT:1776<216:REDLOG (REDUCE LOGIC),15.);(SAT:1954<241:REDLOG (REDUCE LOGIC),15.);(SAT:1957<241:REDLOG (REDUCE LOGIC),15.);(SAT:1958<241:REDLOG (REDUCE LOGIC),15.);(SAT:1959<241:REDLOG (REDUCE LOGIC),15.);(SAT:1960<241:REDLOG (REDUCE LOGIC),15.);(SAT:1962<241:REDLOG (REDUCE LOGIC),15.);(SAT:1963<241:REDLOG (REDUCE LOGIC),15.);(SAT:1964<241:REDLOG (REDUCE LOGIC),15.);(SAT:1974<242:REDLOG (REDUCE LOGIC),15.);(SAT:1983<243:REDLOG (REDUCE LOGIC),15.);(SAT:1991<244:REDLOG (REDUCE LOGIC),15.);(SAT:2000<245:REDLOG (REDUCE LOGIC),15.);(SAT:2009<246:REDLOG (REDUCE LOGIC),15.);(SAT:2010<246:REDLOG (REDUCE LOGIC),15.);(imply:2013<248:REDLOG (REDUCE LOGIC),15.);(imply:2015<249:REDLOG (REDUCE LOGIC),15.);(imply:2017<250:REDLOG (REDUCE LOGIC),15.);(imply:2021<252:REDLOG (REDUCE LOGIC),15.);(SAT:2028<253:REDLOG (REDUCE LOGIC),15.);(SAT:2033<253:REDLOG (REDUCE LOGIC),15.);(SAT:2034<253:REDLOG (REDUCE LOGIC),15.);(SAT:2035<253:REDLOG (REDUCE LOGIC),15.);(SAT:2037<253:REDLOG (REDUCE LOGIC),15.);(SAT:2038<253:REDLOG (REDUCE LOGIC),15.);(SAT:2039<253:REDLOG (REDUCE LOGIC),15.);(imply:2057<254:REDLOG (REDUCE LOGIC),15.);(SAT:2060<254:REDLOG (REDUCE LOGIC),15.);(SAT:2076<256:REDLOG (REDUCE LOGIC),15.);(SAT:2078<256:REDLOG (REDUCE LOGIC),15.);(SAT:2081<256:REDLOG (REDUCE LOGIC),15.);(SAT:2082<256:REDLOG (REDUCE LOGIC),15.);(SAT:2094<257:REDLOG (REDUCE LOGIC),15.);(SAT:2095<257:REDLOG (REDUCE LOGIC),15.);(SAT:2096<257:REDLOG (REDUCE LOGIC),15.);(SAT:2099<257:REDLOG (REDUCE LOGIC),15.);(SAT:2100<257:REDLOG (REDUCE LOGIC),15.);(SAT:2127<260:REDLOG (REDUCE LOGIC),15.);(SAT:2132<260:REDLOG (REDUCE LOGIC),15.);(SAT:2160<265:REDLOG (REDUCE LOGIC),15.);(SAT:2167<267:REDLOG (REDUCE LOGIC),15.);(SAT:2168<267:REDLOG (REDUCE LOGIC),15.);(SAT:2169<267:REDLOG (REDUCE LOGIC),15.);(SAT:2170<267:REDLOG (REDUCE LOGIC),15.);(SAT:2173<267:REDLOG (REDUCE LOGIC),15.);(SAT:2174<267:REDLOG (REDUCE LOGIC),15.);(SAT:2182<269:REDLOG (REDUCE LOGIC),15.);(SAT:2183<269:REDLOG (REDUCE LOGIC),15.);(SAT:2187<269:REDLOG (REDUCE LOGIC),15.);(SAT:2198<271:REDLOG (REDUCE LOGIC),15.);(SAT:2209<273:REDLOG (REDUCE LOGIC),15.);(SAT:2222<275:REDLOG (REDUCE LOGIC),15.);(SAT:2240<277:REDLOG (REDUCE LOGIC),15.);(SAT:2242<277:REDLOG (REDUCE LOGIC),15.);(SAT:2243<277:REDLOG (REDUCE LOGIC),15.);(SAT:2245<277:REDLOG (REDUCE LOGIC),15.);(SAT:2246<277:REDLOG (REDUCE LOGIC),15.);(SAT:2247<277:REDLOG (REDUCE LOGIC),15.);(SAT:2279<283:REDLOG (REDUCE LOGIC),15.);(SAT:2324<288:REDLOG (REDUCE LOGIC),15.);(SAT:2332<290:REDLOG (REDUCE LOGIC),15.);(SAT:2334<290:REDLOG (REDUCE LOGIC),15.);(imply:2338<292:REDLOG (REDUCE LOGIC),15.);(imply:2344<293:REDLOG (REDUCE LOGIC),15.);(SAT:2348<293:REDLOG (REDUCE LOGIC),15.);(SAT:2349<293:REDLOG (REDUCE LOGIC),15.);(SAT:2351<293:REDLOG (REDUCE LOGIC),15.);(SAT:2353<293:REDLOG (REDUCE LOGIC),15.);(SAT:2363<294:REDLOG (REDUCE LOGIC),15.);(SAT:2364<294:REDLOG (REDUCE LOGIC),15.);(SAT:2365<294:REDLOG (REDUCE LOGIC),15.);(SAT:2366<294:REDLOG (REDUCE LOGIC),15.);(SAT:2369<294:REDLOG (REDUCE LOGIC),15.);(SAT:2370<294:REDLOG (REDUCE LOGIC),15.);(imply:2634<330:REDLOG (REDUCE LOGIC),15.);(SAT:2641<331:REDLOG (REDUCE LOGIC),15.);(SAT:2656<333:REDLOG (REDUCE LOGIC),15.);(SAT:2677<336:REDLOG (REDUCE LOGIC),15.);(SAT:2686<337:REDLOG (REDUCE LOGIC),15.);(SAT:2694<338:REDLOG (REDUCE LOGIC),15.);(SAT:2703<339:REDLOG (REDUCE LOGIC),15.);(SAT:2712<340:REDLOG (REDUCE LOGIC),15.);(SAT:2713<340:REDLOG (REDUCE LOGIC),15.);(SAT:2730<342:REDLOG (REDUCE LOGIC),15.);(SAT:2731<342:REDLOG (REDUCE LOGIC),15.);(SAT:2732<342:REDLOG (REDUCE LOGIC),15.);(SAT:2734<342:REDLOG (REDUCE LOGIC),15.);(SAT:2735<342:REDLOG (REDUCE LOGIC),15.);(SAT:2736<342:REDLOG (REDUCE LOGIC),15.);(SAT:2746<343:REDLOG (REDUCE LOGIC),15.);(SAT:2755<344:REDLOG (REDUCE LOGIC),15.);(SAT:2763<345:REDLOG (REDUCE LOGIC),15.);(SAT:2772<346:REDLOG (REDUCE LOGIC),15.);(SAT:2781<347:REDLOG (REDUCE LOGIC),15.);(SAT:2782<347:REDLOG (REDUCE LOGIC),15.);(SAT:2799<349:REDLOG (REDUCE LOGIC),15.);(SAT:2800<349:REDLOG (REDUCE LOGIC),15.);(SAT:2801<349:REDLOG (REDUCE LOGIC),15.);(SAT:2803<349:REDLOG (REDUCE LOGIC),15.);(SAT:2804<349:REDLOG (REDUCE LOGIC),15.);(SAT:2805<349:REDLOG (REDUCE LOGIC),15.);(SAT:2815<351:REDLOG (REDUCE LOGIC),15.);(SAT:2820<351:REDLOG (REDUCE LOGIC),15.);(SAT:2827<353:REDLOG (REDUCE LOGIC),15.);(SAT:2828<353:REDLOG (REDUCE LOGIC),15.);(SAT:2838<356:REDLOG (REDUCE LOGIC),15.);(SAT:2839<356:REDLOG (REDUCE LOGIC),15.);(SAT:2840<356:REDLOG (REDUCE LOGIC),15.);(SAT:2844<356:REDLOG (REDUCE LOGIC),15.);(SAT:2854<357:REDLOG (REDUCE LOGIC),15.);(SAT:2856<357:REDLOG (REDUCE LOGIC),15.);(SAT:2859<357:REDLOG (REDUCE LOGIC),15.);(SAT:2873<359:REDLOG (REDUCE LOGIC),15.);(SAT:2874<359:REDLOG (REDUCE LOGIC),15.);(SAT:2884<360:REDLOG (REDUCE LOGIC),15.);(SAT:2893<361:REDLOG (REDUCE LOGIC),15.);(SAT:2902<362:REDLOG (REDUCE LOGIC),15.);(SAT:2910<363:REDLOG (REDUCE LOGIC),15.);(SAT:2919<364:REDLOG (REDUCE LOGIC),15.);(SAT:2928<365:REDLOG (REDUCE LOGIC),15.);(SAT:2929<365:REDLOG (REDUCE LOGIC),15.);(SAT:2967<371:REDLOG (REDUCE LOGIC),15.);(SAT:2976<372:REDLOG (REDUCE LOGIC),15.);(SAT:2985<373:REDLOG (REDUCE LOGIC),15.);(SAT:2993<374:REDLOG (REDUCE LOGIC),15.);(SAT:3002<375:REDLOG (REDUCE LOGIC),15.);(SAT:3011<376:REDLOG (REDUCE LOGIC),15.);(SAT:3012<376:REDLOG (REDUCE LOGIC),15.);(SAT:3068<385:REDLOG (REDUCE LOGIC),15.);(SAT:3073<385:REDLOG (REDUCE LOGIC),15.);(SAT:3079<387:REDLOG (REDUCE LOGIC),15.);(SAT:3088<388:REDLOG (REDUCE LOGIC),15.);(SAT:3097<389:REDLOG (REDUCE LOGIC),15.);(SAT:3105<390:REDLOG (REDUCE LOGIC),15.);(SAT:3114<391:REDLOG (REDUCE LOGIC),15.);(SAT:3123<392:REDLOG (REDUCE LOGIC),15.);(SAT:3124<392:REDLOG (REDUCE LOGIC),15.);(SAT:3129<394:REDLOG (REDUCE LOGIC),15.);(SAT:3130<394:REDLOG (REDUCE LOGIC),15.);(SAT:3180<403:REDLOG (REDUCE LOGIC),15.);(SAT:3186<403:REDLOG (REDUCE LOGIC),15.);(SAT:3193<405:REDLOG (REDUCE LOGIC),15.);(SAT:3205<407:REDLOG (REDUCE LOGIC),15.);(SAT:3206<407:REDLOG (REDUCE LOGIC),15.);(SAT:3216<410:REDLOG (REDUCE LOGIC),15.);(SAT:3218<410:REDLOG (REDUCE LOGIC),15.);(SAT:3222<410:REDLOG (REDUCE LOGIC),15.);(SAT:3228<411:REDLOG (REDUCE LOGIC),15.);(SAT:3273<416:REDLOG (REDUCE LOGIC),15.);(SAT:3279<419:REDLOG (REDUCE LOGIC),15.);(SAT:3324<424:REDLOG (REDUCE LOGIC),15.);(SAT:3334<426:REDLOG (REDUCE LOGIC),15.);(SAT:3343<427:REDLOG (REDUCE LOGIC),15.);(SAT:3351<428:REDLOG (REDUCE LOGIC),15.);(SAT:3360<429:REDLOG (REDUCE LOGIC),15.);(SAT:3369<430:REDLOG (REDUCE LOGIC),15.);(SAT:3378<431:REDLOG (REDUCE LOGIC),15.);(SAT:3387<432:REDLOG (REDUCE LOGIC),15.);(SAT:3395<433:REDLOG (REDUCE LOGIC),15.);(SAT:3404<434:REDLOG (REDUCE LOGIC),15.);(SAT:3413<435:REDLOG (REDUCE LOGIC),15.);(SAT:3414<435:REDLOG (REDUCE LOGIC),15.);(SAT:3417<436:REDLOG (REDUCE LOGIC),15.);(SAT:3426<437:REDLOG (REDUCE LOGIC),15.);(SAT:3435<438:REDLOG (REDUCE LOGIC),15.);(SAT:3443<439:REDLOG (REDUCE LOGIC),15.);(SAT:3452<440:REDLOG (REDUCE LOGIC),15.);(SAT:3461<441:REDLOG (REDUCE LOGIC),15.);(SAT:3470<442:REDLOG (REDUCE LOGIC),15.);(SAT:3479<443:REDLOG (REDUCE LOGIC),15.);(SAT:3487<444:REDLOG (REDUCE LOGIC),15.);(SAT:3496<445:REDLOG (REDUCE LOGIC),15.);(SAT:3505<446:REDLOG (REDUCE LOGIC),15.);(SAT:3506<446:REDLOG (REDUCE LOGIC),15.);(SAT:3509<447:REDLOG (REDUCE LOGIC),15.)])
Total verification time: 3.348209 second(s)
	Time spent in main process: 0.868054 second(s)
	Time spent in child processes: 2.480155 second(s)"""
  @Test
  def splitOnRegexLemmasTest() = {
    var count = 0
    assertFalse(SleekTestCaseData.lemmaOutput.matches("Entail\\s\\d\\d:\\s.*Valid.*|Entail\\s\\d\\d:\\s.*Fail.*"))
    val splitResults = SleekTestCaseData.lemmaOutput.split("\n")
    for (line <- splitResults)
      if (line.stripMargin.matches("Entailing lemma.*:\\sValid.*|Entailing lemma.*:\\sFail.*"))
        count += 1
    assertEquals(2, count)
    assertTrue(SleekTestCaseData.lemmaOutput.contains("Entail"))
  }

  @Test
  def barrierRegexTest() = {
    val tokenized = barrierDynamicOutput.split("\n")
    var count = 0
    for (token <- tokenized)
      if (token.matches(this.regex))
        count += 1
      else println(token)
    assertEquals(27, count)
    assertTrue("Entail 1: Valid.".matches(this.regex))
  }

  @Test
  def sleekTestCaseTest() = {
    val lemmasLsegTest = new SleekTestCase(
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/lseg.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_lseg" checkAgainst "Valid, Valid")
    lemmasLsegTest.parse(SleekTestCaseData.lemmaOutput, "Entailing lemma.*:\\sValid.*|Entailing lemma.*:\\sFail.*", "\n")
    val generatedResults = lemmasLsegTest.generateTestResult._1
    assertEquals(None, generatedResults)
    assertTrue("Entail 1: Valid ".matches(regex))
  }

  @Test
  def barrierDynamic2Test() = {
    val barrierDynamic2Test = new SleekTestCase(
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-dynamic2.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_dynamic2" checkAgainst "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail, Valid")
    barrierDynamic2Test.parse(barrierDynamicOutput, this.regex, "\n")
    println(barrierDynamic2Test.results)
    val barrierDynamicResults = barrierDynamic2Test.generateTestResult._1
    println(barrierDynamicResults)
  }

  @Test
  def veribsyncBarrierStaticRegexTest() = {
    val tokenized = SleekTestCaseData.veribsyncBarrierStaticOutput.split("\n")
    var count = 0
    for (token <- tokenized)
      if (token.matches(this.regex))
        count += 1
      else println(token)
    assertEquals(5, count)
  }

  @Test
  def veribsyncBarrierStaticTest() = {
    val veribsync_barrier_staticTest = new SleekTestCase(
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-static.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_static" checkAgainst "Valid, Valid, Valid, Valid, Valid")
    veribsync_barrier_staticTest.parse(SleekTestCaseData.veribsyncBarrierStaticOutput, this.regex, "\n")
    val result = veribsync_barrier_staticTest.generateTestResult._1
    assertEquals(None, result)
  }
}