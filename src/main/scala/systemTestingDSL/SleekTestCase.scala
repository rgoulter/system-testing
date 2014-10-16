package systemTestingDSL

import scala.collection.mutable.MutableList

case class SleekTestCase(commandName: String, outputFile: String = "", currentWorkingDirectory: String = ".", fileName: String = "", arguments: String = "", expectedOutput: String = "")
  extends Runnable with Parser {

  var results: MutableList[String] = MutableList()
  def process(source: String, rule: String): Unit = {
    // println("Adding to List: " + rule)
    results += rule
  }

  def run() = this execute

  def generateOutput() = {
    this.parse(run, "Entail\\s.*.*Valid.*|.*Fail.*", DEFAULT_DELIMITER)
    generateTestResults()
  }

  def checkResults(): Boolean = {
    val expectedOutputList: Array[String] = expectedOutput.split(DEFAULT_TEST_OUTPUT_SEPARATOR)
    if (expectedOutputList.size != results.size)
      return false
    for((x, i) <- results.view.zipWithIndex)
      if(!x.contains(expectedOutputList(i)))
        return false
    return true
  }

  def generateTestResults(): Unit = if (checkResults()) println("Passed") else println("Failed")

}
