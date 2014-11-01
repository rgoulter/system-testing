package systemTestingDSL

object Usage {
  def main(args: Array[String]): Unit = {

    //    val sleek9Test = new SleekTestCase(commandName = "sleek", fileName = "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk", arguments = "--elp", outputFileName = "test.out", expectedOutput = "Valid,Fail,Valid,Valid")
    val sleek9Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile "/home/rohit/hg/sleek_hip/examples/working/sleek/sleek9.slk" withArguments "--elp" storeOutputIn "sleek9.out" checkAgainst "Valid, Fail, Valid, Valid"
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
