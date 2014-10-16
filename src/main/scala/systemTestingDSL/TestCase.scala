package systemTestingDSL

case class TestCase (commandName: String, outputFile: String = "",currentWorkingDirectory: String = "." , fileName: String = "", arguments: String = "")
extends Runnable with Parser {

  def process(source: String, rule: String): Unit = println(source + "|" + rule)
  def run() = this execute

}