package systemTestingDSL

object SleekTestSuiteUsage {

  def main(args: Array[String]): Unit = {
    val suite = new SleekTestSuite()
    suite.addTest("sleek", "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek.slk", "", "results", "sleek.out", "Valid, Valid, Valid, Fail")
  }
}