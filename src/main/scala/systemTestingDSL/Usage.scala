package systemTestingDSL

import systemTestingDSL.matchers._

object Usage {
  def main(args: Array[String]): Unit = {

    val sleek9Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk" withArguments "--elp" storeOutputInDirectory "results" withOutputFileName "sleek9.out" checkAgainst "Valid, Fail, Valid, Valid"
//      new SleekTestCaseBuilder.runCommand("sleek").onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk" withArguments "--elp" storeOutputInDirectory "results" withOutputFileName "sleek9.out" checkAgainst "Valid, Fail, Valid, Valid"

      sleek9Test.build generateOutput

    println(new DiffMatcher("Test/1.md", "Test/2.md").diff)
    val genericTestSuite = new GenericTestSuite(defaultCommand = "sleek",
      parentDirectoryName = "/home/rohit/hg/sleek_hip/tinf",
      inputFileExtension = ".*\\.slk",
      outputFileExtension = ".out",
      defaultOptions = "",
      outputFileDirectory = "results/")
    genericTestSuite.generateTestScript()
    genericTestSuite run
  }
}
