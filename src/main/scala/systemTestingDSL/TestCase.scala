package systemTestingDSL

case class TestCase (commandName: String, outputFile: String = "",currentWorkingDirectory: String = "." , fileName: String = "", arguments: String = "")
extends Runnable with Parser {

  def process(s: String): Unit = println(s)
  def run() = this execute

}