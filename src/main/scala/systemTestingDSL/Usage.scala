package systemTestingDSL

object Usage {
  def main(args: Array[String]): Unit = {
    //		val sleek8Test = new SleekTestCase(commandName = "sleek", fileName  = "/home/rohit/hg/sleek_hip/examples/sleek8.slk", arguments = "--elp", outputFile = "test.out", expectedOutput = "Valid")
    //		sleek8Test generateTestResults()
    val ls = new SleekTestCase(commandName = "ls", fileName = "/dev", arguments = "-lart", outputFile = "test.out", expectedOutput = "rohitmukherjee,rohitmukherjee,rohitmukherjee")
    ls generateOutput()
  }
}
