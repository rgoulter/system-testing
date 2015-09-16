package edu.nus.systemtesting.hipsleek

object SVCompTestSuiteUsage {
  def run(): Unit = {
    SVCompTestSuite(directory = """/home/rohit/hg/sleek_hip/svcomp14/termination-crafted""") runAllTests ()
    //    SVCompTestSuite(directory = """/home/rohit/hg/sleek_hip/svcomp14/termination-crafted-lit""") runAllTests ()
    //    SVCompTestSuite(directory = """/home/rohit/hg/sleek_hip/svcomp14/termination-numeric""") runAllTests ()
    //    SVCompTestSuite(directory = """/home/rohit/hg/sleek_hip/svcomp14/termination-memory-alloca""") runAllTests ()
  }
}
