package edu.nus.systemtesting.hipsleek

import com.typesafe.config.ConfigFactory
import edu.nus.systemtesting.hipsleek.SVCompTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SleekTestSuiteUsage
import edu.nus.systemtesting.hipsleek.HipTestSuiteUsage
import edu.nus.systemtesting.hipsleek.HipSleekPreparation

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      showHelpText
      return
    }

    val command = args(0)
    command match {
      case "sleek" => runSleekTests
      case "hip" => runHipTests
      case "all" => runAllTests
      case "svcomp" => runSVCompTests
      case _ => showHelpText
    }
  }

  private def runAllTests(): Unit = {
    runSleekTests
    runHipTests
  }

  private def runSleekTests(): Unit = {
    val config = ConfigFactory.load()

    // Prepare the repo, if necessary
    println("Preparing repo...")

    val REPO_DIR = config.getString("REPO_DIR")
    val rev = None
    val prep = new HipSleekPreparation(REPO_DIR, rev)
    val (prepWorked, prepRemarks) = prep.prepare()

    prepRemarks.foreach(println)
    println

    if (!prepWorked) {
      // abort
      return
    }

    printHeader("Running Sleek Tests")
    val projectDir = REPO_DIR
    val command = projectDir + "sleek"
    val examples = projectDir + "examples/working/sleek/"
    val revision = "???"
    new SleekTestSuiteUsage(config, command, examples, revision).run()
  }

  private def runHipTests(): Unit = {
    val config = ConfigFactory.load()

    // Prepare the repo, if necessary
    println("Preparing repo...")

    val REPO_DIR = config.getString("REPO_DIR")
    val rev = None
    val prep = new HipSleekPreparation(REPO_DIR, rev)
    val (prepWorked, prepRemarks) = prep.prepare()

    prepRemarks.foreach(println)
    println

    if (!prepWorked) {
      // abort
      return
    }

    printHeader("Running Hip Tests")
    val projectDir = REPO_DIR
    val command = projectDir + "hip"
    val examples = projectDir + "examples/working/hip/"
    val revision = "???"
    new HipTestSuiteUsage(config, command, examples, revision).run()
  }

  private def runSVCompTests(): Unit = {
    printHeader("Running SVComp Tests")
    SVCompTestSuiteUsage.run()
  }

  private def showHelpText(): Unit = {
    println(error("Supported Options: sbt run [sleek/hip/all]"))
  }

  private def printHeader(header: String) = {
    println(success("******************"))
    println(success(header))
    println(success("******************"))
  }

  private def error(errorText: String): String =
    Console.CYAN + errorText + Console.RESET + '\n'

  private def success(successText: String): String =
    Console.GREEN + successText + Console.RESET + '\n'
}
