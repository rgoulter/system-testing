package systemTestingDSL

import systemTestingDSL.matchers._

object Usage {
  def main(args: Array[String]): Unit = {

    val sleek9Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk" withArguments "--elp" storeOutputInDirectory "results" withOutputFileName "sleek9.out" checkAgainst "Valid, Fail, Valid, Valid"

    val cll_dTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/cll-d.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "cll_d.out" checkAgainst "Valid"
    cll_dTest.build generateOutput ()

    val label_basicTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/label-basic.slk" withArguments " --dis-eps" storeOutputInDirectory "results" withOutputFileName "label_basic.out" checkAgainst "Fail, Valid, Valid, Fail"
    label_basicTest.build generateOutput ()

    val label_dllTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/label-dll.slk" withArguments " --dis-eps" storeOutputInDirectory "results" withOutputFileName "label_dll.out" checkAgainst "Fail, Valid, Valid, Valid"
    label_dllTest.build generateOutput ()

    val sleek1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek1.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek1.out" checkAgainst "Fail"
    sleek1Test.build generateOutput ()

    val sleek10Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek10.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek10.out" checkAgainst "Valid, Fail"
    sleek10Test.build generateOutput ()

    val sleek2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek2.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek2.out" checkAgainst "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail"
    sleek2Test.build generateOutput ()

    val sleek3Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek3.slk" withArguments " --elp" storeOutputInDirectory "results" withOutputFileName "sleek3.out" checkAgainst "Valid,"
    sleek3Test.build generateOutput ()

    val sleek4Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek4.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek4.out" checkAgainst "Valid, Valid"
    sleek4Test.build generateOutput ()

    val sleek6Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek6.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek6.out" checkAgainst "Valid, Valid"
    sleek6Test.build generateOutput ()

    val sleek7Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek7.slk" withArguments "  --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "sleek7.out" checkAgainst "Valid,"
    sleek7Test.build generateOutput ()

    val sleek8Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek8.slk" withArguments "  --dis-lem-gen " storeOutputInDirectory "results" withOutputFileName "sleek8.out" checkAgainst "Valid,"
    sleek8Test.build generateOutput ()

    val sleek12_lendTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek12-lend.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek12_lend.out" checkAgainst "Valid, Fail, Valid"
    sleek12_lendTest.build generateOutput ()

    val sleek13_lendTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek13-lend.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek13_lend.out" checkAgainst "Valid, Valid, Valid, Fail"
    sleek13_lendTest.build generateOutput ()

    val lst_under1Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lst-under1.slk" withArguments " --inv-test" storeOutputInDirectory "results" withOutputFileName "lst_under1.out" checkAgainst "Valid, Fail,"
    lst_under1Test.build generateOutput ()

    val lst_under2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/lst-under2.slk" withArguments " --inv-test" storeOutputInDirectory "results" withOutputFileName "lst_under2.out" checkAgainst "Fail, Valid"
    lst_under2Test.build generateOutput ()

    val ll_under1aTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1a.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1a.out" checkAgainst "Valid, Valid,"
    ll_under1aTest.build generateOutput ()

    val ll_under1bTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1b.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1b.out" checkAgainst "Fail, Valid,"
    ll_under1bTest.build generateOutput ()

    val ll_under1cTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1c.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1c.out" checkAgainst "Valid, Fail,"
    ll_under1cTest.build generateOutput ()

    val ll_under1dTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1d.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1d.out" checkAgainst "Valid, Valid,"
    ll_under1dTest.build generateOutput ()

    val ll_under1eTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1e.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1e.out" checkAgainst "Fail, Fail,"
    ll_under1eTest.build generateOutput ()

    val ll_under1fTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/ll-under1f.slk" withArguments "  --inv-test --baga-xpure " storeOutputInDirectory "results" withOutputFileName "ll_under1f.out" checkAgainst "Valid, Fail,"
    ll_under1fTest.build generateOutput ()

    val baga_test_epsTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/baga-test-eps.slk" withArguments " --eps" storeOutputInDirectory "results" withOutputFileName "baga_test_eps.out" checkAgainst "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid"
    baga_test_epsTest.build generateOutput ()

    val baga_testTest =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/baga-test.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "baga_test.out" checkAgainst "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid"
    baga_testTest.build generateOutput ()

    //println(new DiffMatcher("Test/1.md", "Test/2.md").diff)
    //    val genericTestSuite = new GenericTestSuite(defaultCommand = "sleek",
    //      parentDirectoryName = "/home/rohit/hg/sleek_hip/tinf",
    //      inputFileExtension = ".*\\.slk",
    //      outputFileExtension = ".out",
    //      defaultOptions = "",
    //      outputFileDirectory = "results/",
    //      generatedTestScriptName = "testRun1.sh")
    //genericTestSuite.generateTestScript()
    //genericTestSuite run
  }
}
