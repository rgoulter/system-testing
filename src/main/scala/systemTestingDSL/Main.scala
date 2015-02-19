package systemTestingDSL

object Main {
  def main(args: Array[String]): Unit = {
    val command = args(0)
    command match {
      case "sleek" => SleekTestSuiteUsage run
      case "hip"   => println("Not implemented yet")
      case _       => println("Unsupported Operation")
    }
  }
}