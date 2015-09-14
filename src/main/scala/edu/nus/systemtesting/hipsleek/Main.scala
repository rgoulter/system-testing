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

    val config = ConfigFactory.load()

    val repoDir = config.getString("REPO_DIR")
    val rev = None

    val command = args(0)
    command match {
      case "sleek" => runSleekTests(repoDir, rev)
      case "hip" => runHipTests(repoDir, rev)
      case "all" => runAllTests(repoDir, rev)
      case "svcomp" => runSVCompTests
      case _ => showHelpText
    }
  }

  private def prepareRepo(repoDir : String, rev : Option[String]): (Boolean, HipSleekPreparation) = {

    // Prepare the repo, if necessary
    println("Preparing repo...")

    val prep = new HipSleekPreparation(repoDir, rev)
    val (prepWorked, prepRemarks) = prep.prepare()

    prepRemarks.foreach(println)
    println

    (prepWorked, prep)
  }

  private def runAllTests(repoDir : String, rev : Option[String]): Unit = {
    val (prepWorked, prep) = prepareRepo(repoDir, rev)

    if (!prepWorked)
      return

    runPreparedSleekTests(prep)
    runPreparedHipTests(prep)
  }

  private def runSleekTests(repoDir : String, rev : Option[String]): Unit = {
    val (prepWorked, prep) = prepareRepo(repoDir, rev)

    if (!prepWorked)
      return

    runPreparedSleekTests(prep)
  }

  private def runPreparedSleekTests(prep: HipSleekPreparation): Unit = {
    val config = ConfigFactory.load()

    printHeader("Running Sleek Tests")
    val projectDir = prep.projectDir
    val command = projectDir + "sleek"
    val examples = projectDir + "examples/working/sleek/"
    val revision = prep.revision
    new SleekTestSuiteUsage(config, command, examples, revision).run()
  }

  private def runHipTests(repoDir : String, rev : Option[String]): Unit = {
    val (prepWorked, prep) = prepareRepo(repoDir, rev)

    if (!prepWorked)
      return

    runPreparedSleekTests(prep)
  }

  private def runPreparedHipTests(prep: HipSleekPreparation): Unit = {
    val config = ConfigFactory.load()

    printHeader("Running Hip Tests")
    val projectDir = prep.projectDir
    val command = projectDir + "hip"
    val examples = projectDir + "examples/working/hip/"
    val revision = prep.revision
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
