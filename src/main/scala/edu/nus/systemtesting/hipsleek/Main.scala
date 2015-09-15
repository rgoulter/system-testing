package edu.nus.systemtesting.hipsleek

import com.typesafe.config.ConfigFactory
import java.nio.file.Paths
import edu.nus.systemtesting.hg.Repository
import java.nio.file.Files

case class CommandLineOptions(command: String = "none")

object Main {
  // Use scopt to parse command-line arguments
  val optParser = new scopt.OptionParser[CommandLineOptions]("system-tests") {
    head("run-system-tests", "0.3.0-SNAPSHOT")
    help("help") text("prints this usage text")
    version("version")
    cmd("sleek") action { (_, c) =>
        c.copy(command = "sleek") } text("  run sleek test cases")
    cmd("hip") action { (_, c) =>
        c.copy(command = "hip") } text("  run hip test cases")
    cmd("all") action { (_, c) =>
        c.copy(command = "all") } text("  run sleek and hip test cases")
    cmd("svcomp") action { (_, c) =>
        c.copy(command = "svcomp") } text("  run svcomp test cases")
  }

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load()

    val repoDir = null; // config.getString("REPO_DIR")
    val rev = None

    optParser.parse(args, CommandLineOptions()) match {
      case Some(config) => {
        // do stuff
        config.command match {
          case "sleek" => runSleekTests(repoDir, rev)
          case "hip" => runHipTests(repoDir, rev)
          case "all" => runAllTests(repoDir, rev)
          case "svcomp" => runSVCompTests
          case _ => showHelpText
        }
      }

      case None => ()
    }
  }

  private def runTestsWith(repoDir: String, rev: Option[String])(f: (String, String) => Unit): Unit = {
    val isRepo = Paths.get(repoDir, ".hg").toFile().exists()

    if (isRepo) {
      runTestsWithRepo(repoDir, rev)(f)
    } else {
      runTestsWithFolder(repoDir, rev)(f)
    }
  }

  /**
   * `repoDir` is assumed to be a repository. This method creates an archive
   * of the repo in a tmp directory if the revision is specified, or
   * if the repository is 'clean'.
   *
   * If the repository is dirty, the repository's working directory is used
   * to run, and it is assumed that this folder can be used to make, and
   * run the tests in.
   */
  private def runTestsWithRepo(repoDir: String, rev: Option[String])(f: (String, String) => Unit): Unit = {
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

        // create archive of repo in tmp
        repo.archive(tmp, rev)

        tmp
    }

    val prep = new HipSleekPreparation(projectDir)
    val (prepWorked, prepRemarks) = prep.prepare()

    prepRemarks.foreach(println)
    println

    // Run the tests
    if (prepWorked)
      f(projectDir, revision)

    // Finished running the tests, clean up.
    tmpDir.toFile().delete()
  }

  /**
   * `projectDir` not assumed to be repository.
   * It *is* assumed that `projectDir` will be used for making, running the
   * executables/tests.
   */
  private def runTestsWithFolder(projectDir: String, rev: Option[String])(f: (String, String) => Unit): Unit = {
    // i.e. LIVE, "in place"
    val revision = rev.getOrElse("unknown")

    // Prepare the repo, if necessary
    println("Preparing folder...")

    val prep = new HipSleekPreparation(projectDir)
    val (prepWorked, prepRemarks) = prep.prepare()

    prepRemarks.foreach(println)
    println

    // Run the tests
    if (prepWorked)
      f(projectDir, revision)

    (prepWorked, projectDir, revision)
  }

  private def runAllTests(repoDir: String, rev: Option[String]): Unit = {
    runTestsWith(repoDir, rev) { (projectDir, revision) =>
      runPreparedSleekTests(projectDir, revision)
      runPreparedHipTests(projectDir, revision)
    }
  }

  private def runSleekTests(repoDir: String, rev: Option[String]): Unit = {
    runTestsWith(repoDir, rev)(runPreparedSleekTests)
  }

  private def runHipTests(repoDir: String, rev: Option[String]): Unit = {
    runTestsWith(repoDir, rev)(runPreparedHipTests)
  }

  /** Assumes that the project dir has been prepared successfully */
  private def runPreparedSleekTests(projectDir: String, revision: String): Unit = {
    val config = ConfigFactory.load()

    printHeader("Running Sleek Tests")
    val command = projectDir + "sleek"
    val examples = projectDir + "examples/working/sleek/"
    val significantTime = 1
    val testCaseTimeout = 300
    new SleekTestSuiteUsage(command, examples, significantTime, testCaseTimeout, revision).run()
  }

  /** Assumes that the project dir has been prepared successfully */
  private def runPreparedHipTests(projectDir: String, revision: String): Unit = {
    val config = ConfigFactory.load()

    printHeader("Running Hip Tests")
    val command = projectDir + "hip"
    val examples = projectDir + "examples/working/hip/"
    val significantTime = 1
    val testCaseTimeout = 300
    new HipTestSuiteUsage(command, examples, significantTime, testCaseTimeout, revision).run()
  }

  private def runSVCompTests(): Unit = {
    printHeader("Running SVComp Tests")
    SVCompTestSuiteUsage.run()
  }

  private def showHelpText(): Unit = {
    println(optParser.usage)
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
