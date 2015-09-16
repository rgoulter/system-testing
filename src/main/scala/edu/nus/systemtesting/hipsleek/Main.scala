package edu.nus.systemtesting.hipsleek

import java.nio.file.{ Files, Paths }

import edu.nus.systemtesting.hg.Repository

object Main {
  def main(args: Array[String]): Unit = {
    println(s"args: ${args.toList}")
    val appCfg = AppConfig.load()

    // Override options from loaded config with CLAs
    AppConfig.CommandLineOptionsParser.parse(args, appCfg) match {
      case Some(config) => {
        import config.{ command, repoDir, rev }

        // do stuff
        command match {
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
    printHeader("Running Sleek Tests")
    val command = Paths.get(projectDir, "sleek").toString()
    val examples = Paths.get(projectDir, "examples/working/sleek/").toString() + "/"
    val significantTime = 1 // CONFIG ME
    val testCaseTimeout = 300
    new SleekTestSuiteUsage(command, examples, significantTime, testCaseTimeout, revision).run()
  }

  /** Assumes that the project dir has been prepared successfully */
  private def runPreparedHipTests(projectDir: String, revision: String): Unit = {
    printHeader("Running Hip Tests")
    val command = Paths.get(projectDir, "hip").toString()
    val examples = Paths.get(projectDir, "examples/working/hip/").toString() + "/"
    val significantTime = 1 // CONFIG ME
    val testCaseTimeout = 300
    new HipTestSuiteUsage(command, examples, significantTime, testCaseTimeout, revision).run()
  }

  private def runSVCompTests(): Unit = {
    printHeader("Running SVComp Tests")
    SVCompTestSuiteUsage.run()
  }

  private def showHelpText(): Unit = {
    println("PRINT USAGE TEXT!!!!")
    println(AppConfig.CommandLineOptionsParser.usage)
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
