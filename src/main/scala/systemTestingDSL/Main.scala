package systemTestingDSL

object Main {

  def main(args: Array[String]): Unit = {
    val command = args(0)
    command match {
      case "sleek" => runSleekTests
      case "hip"   => runHipTests
      case "all"   => runAllTests
      case _       => showHelpText
    }
  }

  private def runAllTests(): Unit = {
    runSleekTests
    runHipTests
  }

  private def runSleekTests(): Unit = {
    println(success("Running Sleek Tests"))
    SleekTestSuiteUsage run
  }

  private def runHipTests(): Unit = HipTestSuiteUsage run

  private def showHelpText(): Unit = {
    println(error("Supported Options: sbt run [sleek/hip/all]"))
  }

  private def error(errorText: String): String = Console.CYAN+ errorText + Console.RESET + '\n'

  private def success(successText: String): String = Console.GREEN + successText + Console.RESET + '\n'

}
