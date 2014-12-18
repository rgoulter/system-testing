package systemTestingDSL

object SleekTestSuiteUsage {

  def main(args: Array[String]): Unit = {
    val suite = new SleekTestSuite()
    // The entries below were generated using the python script parser.py

    suite addTest ("sleek", "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek.slk", " ", "results", "sleek.out", "Valid, Valid, Valid, Fail")
    suite addTest ("sleek", "/home/rohit/hg/sleek_hip/examples/working/sleek/cll-d.slk", " ", "results", "cll_d.out", "Valid")
    suite addTest ("sleek", "/home/rohit/hg/sleek_hip/examples/working/sleek/label-basic.slk", " --dis-eps", "results", "label_basic.out", "Fail, Valid, Valid, Fail")
    suite addTest ("sleek", "/home/rohit/hg/sleek_hip/examples/working/sleek/label-dll.slk", " --dis-eps", "results", "label_dll.out", "Fail, Valid, Valid, Valid")
    suite addTest ("sleek", "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek1.slk", " ", "results", "sleek1.out", "Fail")
    suite.runAllTests
    suite generateTestStatistics

    val cll_dTest =
      new RegressionTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/cll-d.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "cll_d.out"
    println(cll_dTest.build generateTestResults ())
  }
}
