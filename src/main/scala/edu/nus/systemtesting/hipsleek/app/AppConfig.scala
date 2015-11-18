package edu.nus.systemtesting.hipsleek.app

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException
import ConfigDefaults._
import java.nio.file.{ Path, Paths }
import edu.nus.systemtesting.output.OutputVisibility
import edu.nus.systemtesting.output.VisibilityOptions
import edu.nus.systemtesting.hg.Repository

object ConfigDefaults {
  val DefaultTimeout = 300
  val DefaultSignificantTimeThreshold = 1
  val DefaultResultsDir = "results"
  val DefaultBuildFailuresFile = "build_failures"
  val DefaultBinCacheDir = "bincache"
}

/**
 * @param commands which of `hip`, `sleek` should be run.
 * @param timeout is in seconds
 * @param significantTimeThreshold is in seconds
 */
case class AppConfig(repoDir: Option[Path],
                     revs: List[String] = List(),
                     command: String = "none",
                     branchName: Option[String] = None,
                     bisectCmd:  Option[String] = None,
                     bisectFile: Option[String] = None,
                     bisectArgs: List[String] = List(),
                     resultsDir: String = DefaultResultsDir,
                     buildFailuresFile: String = DefaultBuildFailuresFile,
                     binCacheDir: String = DefaultBinCacheDir,
                     timeout: Int = DefaultTimeout,
                     commands: Set[String] = Set(),
                     significantTimeThreshold: Int = DefaultSignificantTimeThreshold,
                     outputVis: OutputVisibility = OutputVisibility.PresetVerbose) {
  def rev(): Option[String] = revs.headOption

  def rev1(): Option[String] = rev

  def rev2(): Option[String] =
    if (revs.length == 2) Some(revs(1)) else None

  def isRunSleek: Boolean =
    commands.contains("sleek")

  def isRunHip: Boolean =
    commands.contains("hip")

  def isRunAll: Boolean =
    Seq("sleek", "hip").forall(commands contains _)

  def repoDirOrDie: Path = {
    val dir: Path = repoDir getOrElse {
      System.err.println(
          """Unable to find REPO_DIR. Try:
            | * Running the program in the mercurial repository, or
            |   a descendant folder of a mercurial repo.
            | * Putting a .hipsleektest.conf file with REPO_DIR=/path/to/repo line
            |   in the current directory, or in some ancestor folder of the CWD.
            | * Compiling this program with an application.conf with REPO_DIR=/path/to/repo line""".stripMargin)
      // 'Fatal' error, quit.
      System.exit(1)
      throw new IllegalStateException
    }

    if (!(dir resolve ".hg").toFile().exists()) {
      System.err.println(s"ERROR! Not a Mercurial repository! REPODIR=$repoDir")
      System.exit(1)
    }

    dir
  }
}

object AppConfig {
  /**
   * Use typesafe's `ConfigFactory` to load settings into this [[AppConfig]].
   *
   * Throws a [[com.typesafe.config.ConfigException]] if required keys in the
   * config (e.g. `REPO_DIR`) are missing.
   *
   * @param maybeRepoDir use this as the repository directory, if not `None`.
   */
  def load(configuration: Config = ConfigFactory.load(),
           maybeRepoDir: Option[Path] = None):
      AppConfig = {
    // Compulsory configuration setting
    val repoDir = try {
      if (!maybeRepoDir.isEmpty) {
        maybeRepoDir
      } else {
        Some(Paths.get(configuration.getString("REPO_DIR")))
      }
    } catch {
      // 'Compulsory', but complain about this elsewhere.
      // @param configuration has lowest precedence for setting REPO_DIR
      case e: ConfigException.Missing => None
    }

    // Optional configuration settings
    // Use defaults if missing from `configuration`
    val timeout = try {
      configuration.getInt("TIMEOUT")
    } catch {
      case e: ConfigException.Missing => DefaultTimeout
    }

    val significantTimeThreshold = try {
      configuration.getInt("SIGNIFICANT_TIME_THRESHOLD")
    } catch {
      case e: ConfigException.Missing => DefaultSignificantTimeThreshold
    }

    val resultsDir = try {
      configuration.getString("RESULTS_DIR")
    } catch {
      case e: ConfigException.Missing => DefaultResultsDir
    }

    val buildFailuresFile = try {
      configuration.getString("BUILD_FAILURES_FILE")
    } catch {
      case e: ConfigException.Missing => DefaultBuildFailuresFile
    }

    val binCacheDir = try {
      configuration.getString("BIN_CACHE_DIR")
    } catch {
      case e: ConfigException.Missing => DefaultBinCacheDir
    }

    AppConfig(repoDir = repoDir,
              resultsDir = resultsDir,
              buildFailuresFile = buildFailuresFile,
              binCacheDir = binCacheDir,
              timeout = timeout,
              significantTimeThreshold = significantTimeThreshold)
  }

  // Use scopt to parse command-line arguments
  val CommandLineOptionsParser = new scopt.OptionParser[AppConfig]("system-tests") {
    import VisibilityOptions.ShowANSI

    head("run-system-tests", "0.6.0-SNAPSHOT")
    help("help") text("prints this usage text")
    version("version")
    opt[Int]('t', "timeout") action { (x, c) =>
      c.copy(timeout = x) } text("timeout time (in seconds) for each individual test case")
    opt[Int]('T', "significant-time") action { (x, c) =>
      c.copy(significantTimeThreshold = x) } text("minimum time (in seconds) for timing results to be shown")
    opt[Unit]("terse") action { (_, c) =>
      c.copy(outputVis = OutputVisibility.PresetSummaryOnly) } text("preset: output shows only the summary")
    opt[Unit]("no-ansi") action { (_, c) =>
      c.copy(outputVis = c.outputVis.copyWith(ShowANSI, false)) } text("output without ANSI codes (i.e. no colour)")
    cmd("sleek") action { (_, c) =>
        c.copy(command = "sleek") } text("run sleek test cases") children(
          arg[String]("<revision>") optional() action { (x, c) =>
          c.copy(revs = List(x)) } text("optional revision of project to test against")
          )
    cmd("hip") action { (_, c) =>
        c.copy(command = "hip") } text("run hip test cases") children(
          arg[String]("<revision>") optional() action { (x, c) =>
          c.copy(revs = List(x)) } text("optional revision of project to test against")
          )
    cmd("all") action { (_, c) =>
        c.copy(command = "all") } text("run sleek and hip test cases") children(
          arg[String]("<revision>") optional() action { (x, c) =>
          c.copy(revs = List(x)) } text("optional revision of project to test against")
          )
    cmd("validate-sleek") action { (_, c) =>
        c.copy(command = "sleek") } text("run automatically-discovered validateable sleek test cases") children(
          )
    cmd("diff") action { (_, c) =>
        c.copy(command = "diff") } text("diff the sleek/hip test results") children(
          opt[Unit]('s', "sleek") action { (_, c) =>
            c.copy(commands = c.commands + "sleek") } text("diff sleek results"),
          opt[Unit]('h', "hip") action { (_, c) =>
            c.copy(commands = c.commands + "hip") } text("diff hip results"),
          opt[Unit]('a', "all") action { (_, c) =>
            c.copy(commands = c.commands + "sleek" + "hip") } text("diff sleek, hip results"),
          opt[Unit]("sleek") action { (_, c) =>
            c.copy(commands = c.commands + "sleek") },
          arg[String]("<rev1 [rev2]>") optional() maxOccurs(2) action { (x, c) =>
          c.copy(revs = c.revs :+ x) } text("optional revisions of project to test against. (old, current)")
          )
    cmd("bisect") action { (_, c) =>
        c.copy(command = "bisect") } text("bisect some TestCase to see when it broke.") children(
          arg[String]("<rev1>") action { (x, c) =>
            c.copy(revs = List(x)) } text("Earlist revision to bisect on. (Test must work here)."),
          arg[String]("<rev2>") action { (x, c) =>
            c.copy(revs = c.revs :+ x) } text("Latest revision to bisect on. (Test must fail here)."),
          arg[String]("<sleek|hip>") action { (x, c) =>
            c.copy(bisectCmd = Some(x)) } text("Test command. ('hip' or 'sleek')."),
          arg[String]("<test file>") action { (x, c) =>
            c.copy(bisectFile = Some(x)) } text("Test file."),
          arg[String]("<test arguments>") unbounded() optional() action { (x, c) =>
            c.copy(bisectArgs = c.bisectArgs :+ x) } text("Test arguments.")
          )
    cmd("status-repo") action { (_, c) =>
        c.copy(command = "status-repo") } text("Report on the status of the repository (all recent branches).") children(
          )
    cmd("status-branch") action { (_, c) =>
        c.copy(command = "status-branch") } text("Report on the status of the current branch.") children(
          arg[String]("[branch name]") optional() action { (x, c) =>
            c.copy(branchName = Some(x)) } text("Name of branch to run status for. Default, if not given.")
          )
    cmd("status") action { (_, c) =>
        c.copy(command = "status") } text("Report on the status of the current commit or working directory.") children(
          )
    checkConfig { c =>
        if (c.command == "diff" && c.commands.isEmpty)
          failure("Must specify --hip, --sleek, or --all to diff command")
        else
          success }
  }
}
