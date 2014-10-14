package systemTestingDSL

case class SleekTestCase(commandName: String, outputFile: String = "", currentWorkingDirectory: String = ".", fileName: String = "", arguments: String = "")
  extends Runnable with Parser {

  def process(s: String): Unit = println(s)
  def run() = this execute
  def generateTestResults() = this.parse(run, "Valid", DEFAULT_DELIMITER )

}