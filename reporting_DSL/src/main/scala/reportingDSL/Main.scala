package reportingDSL

object Usage {
  def main(args: Array[String]): Unit = {
    val firstTest = new Reporter(repository = "/home/rohit/hg/sleek_hip/",
      timePeriod = 300,
      outputDirectoryLocation = "/home/rohit/High-Performance-DSLs/Reporting Tool/",
      outputDirectoryName = "scala_reporting_dsl_results",
      testName = "sleek_tests",
      testCommand = """./sleek.sh""",
      testDirectory = "/home/rohit/High-Performance-DSLs/")
    firstTest.runTests
  }
}
