package systemTestingDSL

object Usage {
  def main(args: Array[String]): Unit = {

    val sleek9Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk" withArguments "--elp" storeOutputInDirectory "results" withOutputFileName "sleek9.out" checkAgainst "Valid, Fail, Valid, Valid"
    sleek9Test.build generateOutput

    val genericTestSuite = new GenericTestSuite(defaultCommand = "sleek",
      parentDirectoryName = "/home/rohit/hg/sleek_hip/tinf",
      inputFileExtension = ".*\\.slk",
      outputFileExtension = ".out",
      defaultOptions = "",
      outputFileDirectory = "results/")
    genericTestSuite run
    //val ls = new SleekTestCase(commandName = "ls", fileName = "/dev", arguments = "-lart", outputFile = "test.out", expectedOutput = "rohitmukherjee,rohitmukherjee,rohitmukherjee")
    // ls generateOutput()
  }
}
