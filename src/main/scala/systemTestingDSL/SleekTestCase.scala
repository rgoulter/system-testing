package systemTestingDSL

case class SleekTestCase(commandName: String, outputFile: String = "", currentWorkingDirectory: String = ".", fileName: String = "", arguments: String = "", expectedOutput: String = "")
  extends Runnable with Parser {

  var results = List()
  def process(s: String):Unit  = s :: results
  def run() = this execute
  def generateOutput() = this.parse(run, "Valid", DEFAULT_DELIMITER )
  def checkResults(): Boolean = results.mkString(".").equals(expectedOutput )
  def generateTestResults(): Unit = if (checkResults()) println ("Passed") else println("Failed")

}