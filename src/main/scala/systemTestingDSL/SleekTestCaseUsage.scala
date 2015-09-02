package systemTestingDSL

object Usage {

  def runSleekTests(): Unit = {
    val sleekTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek" checkAgainst "Valid, Valid, Valid, Fail"
    sleekTest.build generateOutput ()

    val cll_dTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/cll-d.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "cll_d" checkAgainst "Valid"
    cll_dTest.build generateOutput ()

    val label_basicTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/label-basic.slk" withArguments " --dis-eps" storeOutputInDirectory "results" withOutputFileName "label_basic" checkAgainst "Fail, Valid, Valid, Fail"
    label_basicTest.build generateOutput ()

    val label_dllTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/label-dll.slk" withArguments " --dis-eps" storeOutputInDirectory "results" withOutputFileName "label_dll" checkAgainst "Fail, Valid, Valid, Valid"
    label_dllTest.build generateOutput ()

    val sleek1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek1.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek1" checkAgainst "Fail"
    sleek1Test.build generateOutput ()

    val sleek10Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek10.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek10" checkAgainst "Valid, Fail"
    sleek10Test.build generateOutput ()

    val sleek2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek2.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek2" checkAgainst "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail"
    sleek2Test.build generateOutput ()

    val sleek3Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek3.slk" withArguments " --elp" storeOutputInDirectory "results" withOutputFileName "sleek3" checkAgainst "Valid,"
    sleek3Test.build generateOutput ()

    val sleek4Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek4.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek4" checkAgainst "Valid, Valid"
    sleek4Test.build generateOutput ()

    val sleek6Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek6.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek6" checkAgainst "Valid, Valid"
    sleek6Test.build generateOutput ()

    val sleek7Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek7.slk" withArguments "  --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "sleek7" checkAgainst "Valid,"
    sleek7Test.build generateOutput ()

    val sleek8Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek8.slk" withArguments "  --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "sleek8" checkAgainst "Valid,"
    sleek8Test.build generateOutput ()

    val sleek9Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk" withArguments "  --elp " storeOutputInDirectory "results" withOutputFileName "sleek9" checkAgainst "Valid, Valid,"
    sleek9Test.build generateOutput ()

    val sleek12_lendTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek12-lend.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek12_lend" checkAgainst "Valid, Fail, Valid"
    sleek12_lendTest.build generateOutput ()

    val sleek13_lendTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek13-lend.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek13_lend" checkAgainst "Valid, Valid, Valid, Fail"
    sleek13_lendTest.build generateOutput ()

    val lst_under1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lst-under1.slk" withArguments " --inv-test" storeOutputInDirectory "results" withOutputFileName "lst_under1" checkAgainst "Valid, Fail,"
    lst_under1Test.build generateOutput ()

    val lst_under2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lst-under2.slk" withArguments " --inv-test" storeOutputInDirectory "results" withOutputFileName "lst_under2" checkAgainst "Fail, Valid"
    lst_under2Test.build generateOutput ()

    val ll_under1aTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1a.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1a" checkAgainst "Valid, Valid,"
    ll_under1aTest.build generateOutput ()

    val ll_under1bTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1b.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1b" checkAgainst "Fail, Valid,"
    ll_under1bTest.build generateOutput ()

    val ll_under1cTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1c.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1c" checkAgainst "Valid, Fail,"
    ll_under1cTest.build generateOutput ()

    val ll_under1dTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1d.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1d" checkAgainst "Valid, Valid,"
    ll_under1dTest.build generateOutput ()

    val ll_under1eTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1e.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1e" checkAgainst "Fail, Fail,"
    ll_under1eTest.build generateOutput ()

    val ll_under1fTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1f.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1f" checkAgainst "Valid, Fail,"
    ll_under1fTest.build generateOutput ()

    val baga_test_epsTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/baga-test-eps.slk" withArguments " --eps" storeOutputInDirectory "results" withOutputFileName "baga_test_eps" checkAgainst "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid"
    baga_test_epsTest.build generateOutput ()

    val baga_testTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/baga-test.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "baga_test" checkAgainst "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid"
    baga_testTest.build generateOutput ()

    val baga_test_2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/baga-test-2.slk" withArguments " --dis-baga-xpure --dis-eps" storeOutputInDirectory "results" withOutputFileName "baga_test_2" checkAgainst "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Fail"
    baga_test_2Test.build generateOutput ()

    val symb_diffTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/symb-diff.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "symb_diff" checkAgainst "Valid, Valid, Valid"
    symb_diffTest.build generateOutput ()

    val xpure3nodesTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/xpure3nodes.slk" withArguments "" storeOutputInDirectory "results" withOutputFileName "xpure3nodes" checkAgainst "Valid, Valid"
    xpure3nodesTest.build generateOutput ()

    val infer_app_invTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/app-inv.slk" withArguments " --inv --dis-eps" storeOutputInDirectory "results" withOutputFileName "infer_app_inv" checkAgainst "Valid, Valid, Fail, Valid, Valid, Valid"
    infer_app_invTest.build generateOutput ()

    val infer_app_inv2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/app-inv2.slk" withArguments " --inv --dis-eps" storeOutputInDirectory "results" withOutputFileName "infer_app_inv2" checkAgainst "Valid, Valid, Valid, Fail"
    infer_app_inv2Test.build generateOutput ()

    val infer_infer1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer1.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer1" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid"
    infer_infer1Test.build generateOutput ()

    val infer_infer2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer2.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer2" checkAgainst "Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Fail"
    infer_infer2Test.build generateOutput ()

    val infer_infer4Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer4.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer4" checkAgainst "Fail, Fail, Val"
    infer_infer4Test.build generateOutput ()

    val infer_infer5Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer5.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer5" checkAgainst "Valid, Valid, Fail, Valid"
    infer_infer5Test.build generateOutput ()

    val infer_infer5aTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer5a.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer5a" checkAgainst "Fail, Valid"
    infer_infer5aTest.build generateOutput ()

    val infer_infer6Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer6.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer6" checkAgainst "Valid"
    infer_infer6Test.build generateOutput ()

    val infer_infer7Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer7.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer7" checkAgainst "Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid"
    infer_infer7Test.build generateOutput ()

    val infer_infer8Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer8.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer8" checkAgainst "Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid"
    infer_infer8Test.build generateOutput ()

    val infer_infer9Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer9.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer9" checkAgainst "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid"
    infer_infer9Test.build generateOutput ()

    val infer_infer10Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer10.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer10" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Fail, Valid"
    infer_infer10Test.build generateOutput ()

    val infer_infer11Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer11.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer11" checkAgainst "Fail"
    infer_infer11Test.build generateOutput ()

    val infer_infer12Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer12.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer12" checkAgainst "Valid, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid"
    infer_infer12Test.build generateOutput ()

    val infer_infer13Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer13.slk" withArguments " --sa-en-cont" storeOutputInDirectory "results" withOutputFileName "infer_infer13" checkAgainst "Valid, Valid, Valid, Valid, Valid"
    infer_infer13Test.build generateOutput ()

    val infer_infer14Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer14.slk" withArguments " --sa-en-pure-field" storeOutputInDirectory "results" withOutputFileName "infer_infer14" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid"
    infer_infer14Test.build generateOutput ()

    val infer_infer15Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer15.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer15" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid"
    infer_infer15Test.build generateOutput ()

    val infer_infer16Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infer/infer16.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "infer_infer16" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid"
    infer_infer16Test.build generateOutput ()

    val ann2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ann2.slk" withArguments "  --imm --en-imm-inv --etcsu1 " storeOutputInDirectory "results" withOutputFileName "ann2" checkAgainst "Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail"
    ann2Test.build generateOutput ()

    val imm_imm1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm/imm1.slk" withArguments "  --imm --etcsu1 " storeOutputInDirectory "results" withOutputFileName "imm_imm1" checkAgainst "Fail, Valid, Valid, Valid, Valid, Valid"
    imm_imm1Test.build generateOutput ()

    val imm_imm2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm/imm2.slk" withArguments "  --imm --etcsu1 " storeOutputInDirectory "results" withOutputFileName "imm_imm2" checkAgainst "Fail, Valid, Fail, Valid, Fail"
    imm_imm2Test.build generateOutput ()

    val imm_imm3Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm/imm3.slk" withArguments "  --imm --etcsu1 " storeOutputInDirectory "results" withOutputFileName "imm_imm3" checkAgainst "Fail, Fail, Valid, Valid, Valid"
    imm_imm3Test.build generateOutput ()

    val imm_imm4Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm/imm4.slk" withArguments "  --imm --etcsu1 " storeOutputInDirectory "results" withOutputFileName "imm_imm4" checkAgainst "Valid, Fail"
    imm_imm4Test.build generateOutput ()

    val imm_imm_hardTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm/imm-hard.slk" withArguments "  --imm --eps" storeOutputInDirectory "results" withOutputFileName "imm_imm_hard" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid"
    imm_imm_hardTest.build generateOutput ()

    val imm_field_sleek01Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm-field/sleek01.slk" withArguments "  --field-ann --etcsu1 " storeOutputInDirectory "results" withOutputFileName "imm_field_sleek01" checkAgainst "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail"
    imm_field_sleek01Test.build generateOutput ()

    val imm_field_sleek02Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm-field/sleek02.slk" withArguments "  --field-ann --etcsu1 " storeOutputInDirectory "results" withOutputFileName "imm_field_sleek02" checkAgainst "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail"
    imm_field_sleek02Test.build generateOutput ()

    val imm_field_sleek03Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm-field/sleek03.slk" withArguments "  --field-ann --etcsu1 " storeOutputInDirectory "results" withOutputFileName "imm_field_sleek03" checkAgainst "Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail"
    imm_field_sleek03Test.build generateOutput ()

    val epsTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/eps.slk" withArguments "  --dis-imm " storeOutputInDirectory "results" withOutputFileName "eps" checkAgainst "Valid"
    epsTest.build generateOutput ()

    val imm_field_sleek05Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/imm-field/sleek05.slk" withArguments "  --field-ann --etcsu1 " storeOutputInDirectory "results" withOutputFileName "imm_field_sleek05" checkAgainst "Valid, Fail, Fail, Fail, Fail, Fail, Valid, Valid, Val"
    imm_field_sleek05Test.build generateOutput ()

    val classic_classic11Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/classic/classic1.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "classic_classic1" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail"
    classic_classic11Test.build generateOutput ()

    val classic_classic1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/classic/classic1.slk" withArguments "  --classic" storeOutputInDirectory "results" withOutputFileName "classic_classic1" checkAgainst "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail"
    classic_classic1Test.build generateOutput ()

    val classic_classic1bTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/classic/classic1b.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "classic_classic1b" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail"
    classic_classic1bTest.build generateOutput ()

    val classic_classic2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/classic/classic2.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "classic_classic2" checkAgainst "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail"
    classic_classic2Test.build generateOutput ()

    val classic_classic3Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/classic/classic3.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "classic_classic3" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail"
    classic_classic3Test.build generateOutput ()

    val classic_classic4Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/classic/classic4.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "classic_classic4" checkAgainst "Valid, Fail, Valid, Fail, Valid, Fail, Valid, Fail"
    classic_classic4Test.build generateOutput ()

    val inf_no_epsTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/inf-no-eps.slk" withArguments "--dsd --en-inf --dis-eps" storeOutputInDirectory "results" withOutputFileName "inf_no_eps" checkAgainst "Fail, Fail, Valid, Valid"
    inf_no_epsTest.build generateOutput ()

    val infinityTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/infinity.slk" withArguments "--dsd --en-inf --dis-eps" storeOutputInDirectory "results" withOutputFileName "infinity" checkAgainst "Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Valid, Valid"
    infinityTest.build generateOutput ()

    val inflemTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/inflem.slk" withArguments "  --en-inf --elp --dis-eps" storeOutputInDirectory "results" withOutputFileName "inflem" checkAgainst "Valid,"
    inflemTest.build generateOutput ()

    val lemmas_sort2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/sort2.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_sort2" checkAgainst "Fail, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid,"
    lemmas_sort2Test.build generateOutput ()

    val lemmas_lsegTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/lseg.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_lseg" checkAgainst "Valid, Valid, Valid, Valid,"
    lemmas_lsegTest.build generateOutput ()

    val lemmas_lseg1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/lseg1.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_lseg1" checkAgainst "Valid,"
    lemmas_lseg1Test.build generateOutput ()

    val lemmas_rlsegTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/rlseg.slk" withArguments "  --elp --dis-lem-gen" storeOutputInDirectory "results" withOutputFileName "lemmas_rlseg" checkAgainst "Valid, Valid, Valid,"
    lemmas_rlsegTest.build generateOutput ()

    val lemmas_lemma_foldTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/lemma-fold.slk" withArguments "  --elp " storeOutputInDirectory "results" withOutputFileName "lemmas_lemma_fold" checkAgainst "Valid,"
    lemmas_lemma_foldTest.build generateOutput ()

    val lemmas_rd_lem_1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/rd-lem-1.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_rd_lem_1" checkAgainst "Fail, Valid,"
    lemmas_rd_lem_1Test.build generateOutput ()

    val lemmas_rd_lem_2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/rd-lem-2.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "lemmas_rd_lem_2" checkAgainst "Fail"
    lemmas_rd_lem_2Test.build generateOutput ()

    val lemmas_app_tailTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/app-tail.slk" withArguments "  --elp " storeOutputInDirectory "results" withOutputFileName "lemmas_app_tail" checkAgainst "Valid, Valid,"
    lemmas_app_tailTest.build generateOutput ()

    val lemmas_lseg_caseTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/lseg_case.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_lseg_case" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid,"
    lemmas_lseg_caseTest.build generateOutput ()

    val lemmas_llTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/ll.slk" withArguments "  --elp " storeOutputInDirectory "results" withOutputFileName "lemmas_ll" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid,"
    lemmas_llTest.build generateOutput ()

    val lemmas_ll_tailTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/ll_tail.slk" withArguments "  --elp " storeOutputInDirectory "results" withOutputFileName "lemmas_ll_tail" checkAgainst "Valid, Valid"
    lemmas_ll_tailTest.build generateOutput ()

    val lemmas_nlseg3Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/nlseg3.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "lemmas_nlseg3" checkAgainst "Valid, Valid"
    lemmas_nlseg3Test.build generateOutput ()

    val lemmas_nlseg4eTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/nlseg4e.slk" withArguments "  --elp " storeOutputInDirectory "results" withOutputFileName "lemmas_nlseg4e" checkAgainst "Valid, Valid"
    lemmas_nlseg4eTest.build generateOutput ()

    val lemmas_nlseg4e1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/nlseg4e1.slk" withArguments " --dis-imm" storeOutputInDirectory "results" withOutputFileName "lemmas_nlseg4e1" checkAgainst "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid"
    lemmas_nlseg4e1Test.build generateOutput ()

    val lemmas_sll_tailLTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/sll_tailL.slk" withArguments "  --elp " storeOutputInDirectory "results" withOutputFileName "lemmas_sll_tailL" checkAgainst "Valid, Valid"
    lemmas_sll_tailLTest.build generateOutput ()

    val lemmas_dseg_newTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/dseg-new.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_dseg_new" checkAgainst "Valid, Fail, Valid,"
    lemmas_dseg_newTest.build generateOutput ()

    val lemmas_dseg1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/dseg1.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_dseg1" checkAgainst "Valid, Fail,"
    lemmas_dseg1Test.build generateOutput ()

    val lemmas_odd_lsegTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/odd-lseg.slk" withArguments "  --elp --dis-lem-gen --dis-eps" storeOutputInDirectory "results" withOutputFileName "lemmas_odd_lseg" checkAgainst "Valid, Valid,"
    lemmas_odd_lsegTest.build generateOutput ()

    val lemmas_lseg_complexTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lemmas/lseg_complex.slk" withArguments "  --elp --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "lemmas_lseg_complex" checkAgainst "Valid, Valid, Valid"
    lemmas_lseg_complexTest.build generateOutput ()

    val fracperm_split_simpleTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/split_simple.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_split_simple" checkAgainst "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid"
    fracperm_split_simpleTest.build generateOutput ()

    val fracperm_split_combineTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/split-combine.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_split_combine" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid"
    fracperm_split_combineTest.build generateOutput ()

    val vperm_vpermTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/vperm/vperm.slk" withArguments " --ann-vp" storeOutputInDirectory "results" withOutputFileName "vperm_vperm" checkAgainst "Valid, Valid, Fail, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid"
    vperm_vpermTest.build generateOutput ()

    val veribsync_bperm_split_combineTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/bperm-split-combine.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_bperm_split_combine" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid"
    veribsync_bperm_split_combineTest.build generateOutput ()

    val veribsync_barrier_staticTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-static.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_static" checkAgainst "Valid, Valid, Valid, Valid, Valid"
    veribsync_barrier_staticTest.build generateOutput ()

    val veribsync_barrier_dynamic2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-dynamic2.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_dynamic2" checkAgainst "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid"
    veribsync_barrier_dynamic2Test.build generateOutput ()

    val threads_thrd1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/threads/thrd1.slk" withArguments " --en-para --en-thrd-resource -tp redlog" storeOutputInDirectory "results" withOutputFileName "threads_thrd1" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Val"
    threads_thrd1Test.build generateOutput ()

    val conchip_threadsTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/conchip/threads.slk" withArguments " -tp parahip" storeOutputInDirectory "results" withOutputFileName "conchip_threads" checkAgainst "Valid, Valid, Val"
    conchip_threadsTest.build generateOutput ()

    val conchip_latchTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/conchip/latch.slk" withArguments " -tp parahip" storeOutputInDirectory "results" withOutputFileName "conchip_latch" checkAgainst "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fa"
    conchip_latchTest.build generateOutput ()

    val threads_thrd11Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/threads/thrd1.slk" withArguments " --en-para --en-thrd-resource -tp redlog" storeOutputInDirectory "results" withOutputFileName "threads_thrd1" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Val"
    threads_thrd11Test.build generateOutput ()

    val vperm_vpermTest1 =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/vperm/vperm.slk" withArguments " --ann-vp" storeOutputInDirectory "results" withOutputFileName "vperm_vperm" checkAgainst "Valid, Valid, Fail, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid"
    vperm_vpermTest1.build generateOutput ()

    val vperm_vperm2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/vperm/vperm2.slk" withArguments " --ann-vp" storeOutputInDirectory "results" withOutputFileName "vperm_vperm2" checkAgainst "Valid, Valid, Fail"
    vperm_vperm2Test.build generateOutput ()

    val fracperm_sleekTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek" checkAgainst "Valid, Valid, Valid, Fail"
    fracperm_sleekTest.build generateOutput ()

    val fracperm_sleek1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek1.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek1" checkAgainst "Fail"
    fracperm_sleek1Test.build generateOutput ()

    val fracperm_sleek10Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek10.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek10" checkAgainst "Valid, Fail"
    fracperm_sleek10Test.build generateOutput ()

    val fracperm_sleek2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek2.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek2" checkAgainst "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail"
    fracperm_sleek2Test.build generateOutput ()

    val fracperm_sleek3Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek3.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek3" checkAgainst "Valid, Fail, Valid"
    fracperm_sleek3Test.build generateOutput ()

    val fracperm_sleek4Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek4.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek4" checkAgainst "Valid, Valid"
    fracperm_sleek4Test.build generateOutput ()

    val fracperm_sleek6Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek6.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek6" checkAgainst "Valid, Valid"
    fracperm_sleek6Test.build generateOutput ()

    val fracperm_sleek7Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek7.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek7" checkAgainst "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail, Valid"
    fracperm_sleek7Test.build generateOutput ()

    val fracperm_sleek8Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek8.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek8" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail, Valid, Fail"
    fracperm_sleek8Test.build generateOutput ()

    val fracperm_sleek9Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/sleek9.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_sleek9" checkAgainst "Valid, Fail, Valid, Valid"
    fracperm_sleek9Test.build generateOutput ()

    val fracperm_norm1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/norm1.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_norm1" checkAgainst "Fail, Valid, Fail, Valid"
    fracperm_norm1Test.build generateOutput ()

    val fracperm_norm3Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/norm3.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_norm3" checkAgainst "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail"
    fracperm_norm3Test.build generateOutput ()

    val fracperm_norm4Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/norm4.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_norm4" checkAgainst "Valid, Valid, Valid"
    fracperm_norm4Test.build generateOutput ()

    val fracperm_uni_varsTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/uni_vars.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_uni_vars" checkAgainst "Valid, Valid, Fail"
    fracperm_uni_varsTest.build generateOutput ()

    val fracperm_frac1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/frac1.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_frac1" checkAgainst "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid"
    fracperm_frac1Test.build generateOutput ()

    val fracperm_frac2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/frac2.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_frac2" checkAgainst "Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Fail, Fail"
    fracperm_frac2Test.build generateOutput ()

    val fracperm_frac3Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/frac3.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_frac3" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Fail, Fail"
    fracperm_frac3Test.build generateOutput ()

    val fracperm_split_simpleTest1 =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/split_simple.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_split_simple" checkAgainst "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid"
    fracperm_split_simpleTest1.build generateOutput ()

    val fracperm_combine_dataTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/combine_data.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_combine_data" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid"
    fracperm_combine_dataTest.build generateOutput ()

    val fracperm_combine_simpleTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/combine_simple.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_combine_simple" checkAgainst "Valid, Valid, Fail, Valid, Valid, Valid, Valid"
    fracperm_combine_simpleTest.build generateOutput ()

    val fracperm_split_combineTest1 =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/split-combine.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_split_combine" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid"
    fracperm_split_combineTest1.build generateOutput ()

    val fracperm_combine2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/fracperm/combine2.slk" withArguments "--en-para -perm fperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "fracperm_combine2" checkAgainst "Valid, Valid, Valid, Valid"
    fracperm_combine2Test.build generateOutput ()

    val veribsync_bperm1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/bperm1.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_bperm1" checkAgainst "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail"
    veribsync_bperm1Test.build generateOutput ()

    val veribsync_bperm_splitTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/bperm-split.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_bperm_split" checkAgainst "Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid"
    veribsync_bperm_splitTest.build generateOutput ()

    val veribsync_bperm_combineTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/bperm-combine.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_bperm_combine" checkAgainst "Valid, Valid, Valid, Valid"
    veribsync_bperm_combineTest.build generateOutput ()

    val veribsync_bperm_split_combineTest1 =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/bperm-split-combine.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_bperm_split_combine" checkAgainst "Valid, Valid, Valid, Valid, Valid, Valid"
    veribsync_bperm_split_combineTest1.build generateOutput ()

    val veribsync_barrier1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier1.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier1" checkAgainst "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail"
    veribsync_barrier1Test.build generateOutput ()

    val veribsync_barrier_splitTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-split.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_split" checkAgainst "Valid, Valid, Fail, Valid, Valid, Fail, Valid"
    veribsync_barrier_splitTest.build generateOutput ()

    val veribsync_barrier_combineTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-combine.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_combine" checkAgainst "Valid, Valid, Valid, Valid"
    veribsync_barrier_combineTest.build generateOutput ()

    val veribsync_barrier_sepTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-sep.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_sep" checkAgainst "Valid, Valid, Valid"
    veribsync_barrier_sepTest.build generateOutput ()

    val veribsync_barrier_staticTest1 =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-static.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_static" checkAgainst "Valid, Valid, Valid, Valid, Valid"
    veribsync_barrier_staticTest1.build generateOutput ()

    val veribsync_barrier_dynamicTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-dynamic.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_dynamic" checkAgainst "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid"
    veribsync_barrier_dynamicTest.build generateOutput ()

    val veribsync_barrier_dynamic2Test1 =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/veribsync/barrier-dynamic2.slk" withArguments "--en-para -perm bperm -tp redlog" storeOutputInDirectory "results" withOutputFileName "veribsync_barrier_dynamic2" checkAgainst "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid"
    veribsync_barrier_dynamic2Test1.build generateOutput ()

  }
}

