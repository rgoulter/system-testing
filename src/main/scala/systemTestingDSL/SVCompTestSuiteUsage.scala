package systemTestingDSL

import systemTestingDSL.outputGenerator.ConsoleOutputGenerator

object SVCompTestSuiteUsage extends ConsoleOutputGenerator {
  def run(): Unit = {
    SVCompTestSuite(directory = """/home/rohit/hg/sleek_hip/svcomp14""") runAllTests
  }

}