package edu.nus.systemtesting.hipsleek.app

import java.io.File
import java.io.PrintWriter
import java.nio.file.Path
import java.nio.file.Paths
import scala.io.Source
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.hipsleek.BuildResult
import edu.nus.systemtesting.hipsleek.HipTestSuiteUsage
import edu.nus.systemtesting.hipsleek.SleekTestSuiteUsage
import edu.nus.systemtesting.hipsleek.ValidateableSleekTestCase
import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.ExpectsOutput

/**
 * Generates, runs a subset of some set of testables.
 *
 * Does not deal with legacy, misnamed `run-fast-tests.pl`.
 */
class RunFast(config: AppConfig) {
  val repoDirPath = config.repoDirOrDie
  val repoDir = repoDirPath.toFile

  // Each instance of `ConfiguredMain` only ever uses the one `Repository`
  val repo = new Repository(config.repoDirOrDie)


  val runUtils = new RunUtils(config)
  import runUtils.runTestsWith
  val runHipSleek = new RunHipSleek(config)
  import runHipSleek.{ altRunTests, runTestCaseForRevision }


  def DefaultFastCacheName(name: String) = s".fasttests_$name"

  // might make sense to use SQLite db here.

  private case class FastTestRow(filename: String, args: Option[String] = None) {
    override def toString(): String =
      filename + (args map (a => " " + a) getOrElse "")
  }

  private def loadFastTests(file: File): Option[List[FastTestRow]] = {
    if (file.exists) {
      val src = Source fromFile file
      val content = src.mkString
      src.close()

      Some(content.lines.toList map { line =>
        line.split(" ") match {
          case Array(filename) => FastTestRow(filename)
          case Array(filename, args) => FastTestRow(filename, Some(args))
        }
      })
    } else {
      None
    }
  }

  private def saveToFile(file: File, rows: List[FastTestRow]): Unit = {
    val out = new PrintWriter(file)

    rows foreach { row =>
      out.println(row.toString())
    }

    out.flush()
    out.close()
  }

  private def fileForSuite(suite: Suite): File =
    new File(DefaultFastCacheName(suite.toString()))



  private def args(arguments: String): Option[String] =
    if (arguments == "") None else Some(arguments)

  private def rowFromTestable(tc: Testable): FastTestRow = {
    import tc.{ fileName, arguments }

    FastTestRow(fileName.toString(), args(arguments))
  }

  private def filterTestable(all: List[Testable with ExpectsOutput],
                             loaded: List[FastTestRow]): List[Testable with ExpectsOutput] = {
    val loadedSet = loaded.toSet

    all filter { tc => loadedSet contains rowFromTestable(tc) }
  }

  private def loadFastTests(suite: Suite): Option[List[Testable with ExpectsOutput]] = {
    loadFastTests(fileForSuite(suite)) map { fastTests =>
      suite match {
        case HipOnly()   => filterTestable(HipTestSuiteUsage.allTestable, fastTests)
        case SleekOnly() => filterTestable(SleekTestSuiteUsage.allTestable, fastTests)

        // This is a little more dubious,
        // since typically Sleek+Hip are treated distinctly.
        case All() => filterTestable(SleekTestSuiteUsage.allTestable ++ HipTestSuiteUsage.allTestable, fastTests)

        case SleekValidateOnly() => {
          // XXX construct ValidateableSleekTestCase directly
        }
      }
    }
  }

  private def saveFastTests(suite: Suite, data: List[Testable with ExpectsOutput]): Unit = {
    saveToFile(fileForSuite(suite), data map rowFromTestable)
  }

  private def generateFastTestablesForSuite(suite: Suite): List[Testable with ExpectsOutput] = {
    // XXX get the universe of testable for the suite,

    // XXX if we have sufficent number of results, can compute from that,

    // XXX otherwise, must run with a short timeout + don't save results (for T/O),
    // so as to see which tests are "quick"

    List()
  }

  private def suiteFromString(suite: String): Suite =
    suite match {
      case "hip" => HipOnly()
      case "sleek" => SleekOnly()
      case "all" => All()
      case "sleek-validate" => SleekValidateOnly()
      case "validate-sleek" => SleekValidateOnly()
      case _ => throw new IllegalArgumentException(s"Unknown suite: $suite")
    }

  def run(): Unit = {
    import config.{ runFastSuite, runFastGenerate => isForcedGenerate }

    val suiteName = runFastSuite.getOrElse(throw new IllegalArgumentException("must be given a suite name"))

    // more/less expect suiteName to be one of:
    //   sleek
    //   hip
    //   all
    //   validate-sleek
    val suite = suiteFromString(suiteName)

    //
    // Step 1. Generate set of fast tests (if need be)
    //

    val fastTests = loadFastTests(suite) match {
      case Some(xs) if !isForcedGenerate => xs
      case None => {
        val xs = generateFastTestablesForSuite(suite)

        saveFastTests(suite, xs)

        xs
      }
    }

    //
    // Step 2. With the set of generated tests in hand, run these.
    //

    // XXX cf. implementation in Validate's run
    println("TODO: run suite..")
  }
}
