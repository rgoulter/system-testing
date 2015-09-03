package edu.nus.systemtesting

case class TestCase(commandName: String,
  outputFileName: String = "",
  outputDirectory: String = ".",
  fileName: String = "",
  arguments: String = "")
    extends Runnable with Parser {
  def process(source: String, rule: String): Unit = println(source + "|" + rule)

  def run() = this execute

  override def formCommand(): String = {
    commandName.concat(separator).concat(arguments).concat(separator).concat(fileName)
  }
}