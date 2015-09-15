package edu.nus.systemtesting.hipsleek

import com.typesafe.config.ConfigFactory
import edu.nus.systemtesting.hipsleek.SVCompTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SleekTestSuiteUsage
import edu.nus.systemtesting.hipsleek.HipTestSuiteUsage
import edu.nus.systemtesting.hipsleek.HipSleekPreparation
import java.nio.file.Paths
import edu.nus.systemtesting.hg.Repository
import java.nio.file.Files

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

  private def prepareRepo(repoDir : String, rev : Option[String]): (Boolean, String, String) = {
    val isRepo = Paths.get(repoDir, ".hg").toFile().exists()

    if (isRepo) {
      // Prepare the repo, if necessary
      println("Preparing repo...")

      val repo = new Repository(repoDir)
      val revision = rev.getOrElse(repo.identify())

      val isDirty = rev match {
        // Assumes that given rev isn't a "dirty" one;
        // e.g. a call to "hg update <rev>" would make sense
        case Some(s) => false
        // If no rev given, use Working Directory of repo.
        case None => repo.isDirty()
      }

      val tmpDir = Files.createTempDirectory("edunussystest")

      val projectDir = if (isDirty) {
        // i.e. LIVE, "in place"
        repoDir
      } else {
          val tmp = tmpDir.toAbsolutePath().toString()
          repo.archive(tmp, rev) // create archive of repo in tmp
          tmp
      }

      val prep = new HipSleekPreparation(projectDir)
      val (prepWorked, prepRemarks) = prep.prepare()

      prepRemarks.foreach(println)
      println

      // TODO: Unfortunately, this means we don't delete the tmp dir
      // after running the tests.

      (prepWorked, projectDir, revision)
    } else {
      // i.e. LIVE, "in place"
      val projectDir = repoDir
      val revision = rev.getOrElse("unknown")

      // Prepare the repo, if necessary
      println("Preparing folder...")

      val prep = new HipSleekPreparation(projectDir)
      val (prepWorked, prepRemarks) = prep.prepare()

      prepRemarks.foreach(println)
      println

      (prepWorked, projectDir, revision)
    }
  }

  private def runAllTests(repoDir : String, rev : Option[String]): Unit = {
    val (prepWorked, projectDir, revision) = prepareRepo(repoDir, rev)

    if (!prepWorked)
      return

    runPreparedSleekTests(projectDir, revision)
    runPreparedHipTests(projectDir, revision)
  }

  private def runSleekTests(repoDir : String, rev : Option[String]): Unit = {
    val (prepWorked, projectDir, revision) = prepareRepo(repoDir, rev)

    if (!prepWorked)
      return

    runPreparedSleekTests(projectDir, revision)
  }

  private def runPreparedSleekTests(projectDir: String, revision: String): Unit = {
    val config = ConfigFactory.load()

    printHeader("Running Sleek Tests")
    val command = projectDir + "sleek"
    val examples = projectDir + "examples/working/sleek/"
    new SleekTestSuiteUsage(config, command, examples, revision).run()
  }

  private def runHipTests(repoDir : String, rev : Option[String]): Unit = {
    val (prepWorked, projectDir, revision) = prepareRepo(repoDir, rev)

    if (!prepWorked)
      return

    runPreparedSleekTests(projectDir, revision)
  }

  private def runPreparedHipTests(projectDir: String, revision: String): Unit = {
    val config = ConfigFactory.load()

    printHeader("Running Hip Tests")
    val command = projectDir + "hip"
    val examples = projectDir + "examples/working/hip/"
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
