package edu.nus.systemtesting

import scala.sys.process.Process
import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterAll
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import org.scalatest.ConfigMap
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfter
import edu.nus.systemtesting.FileSystemUtilities
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.hipsleek.BuildFailed
import edu.nus.systemtesting.hipsleek.HipSleekPreparation
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.hipsleek.SleekTestCase
import edu.nus.systemtesting.hipsleek.app.AppConfig
import edu.nus.systemtesting.hipsleek.app.RunHipSleek
import com.typesafe.config.ConfigException
import java.nio.file.Paths

/**
 * @author richardg
 */
class DirtyRepoSpec extends FlatSpec with BeforeAndAfter {
  /**
   * A revision known to not have compiler errors
   */
  val KnownGoodRevision = "7ac44bdb0dfd"

  // Assumes presence of a config
  val configuration = ConfigFactory.load()

//  assume(configuration., clue)
  val REPO_DIR = try {
    Paths.get(configuration.getString("REPO_DIR"))
  } catch {
    case e: ConfigException.Missing => {
      cancel("`REPO_DIR` key not in config, cannot test system.", e)
      Paths.get("/path/to/repo")
    }
  }

  val repo = new Repository(REPO_DIR)

  val KnownGoodCommit = repo.identify(Some(KnownGoodRevision))

  var tmpRepoDir: Path = _
  var tmpResultsDir: Path = _
  var tmpBinCacheDir: Path = _

  before {
    // Clone the repo
    tmpRepoDir = Files.createTempDirectory("edunussystestrepo")
    tmpResultsDir = Files.createTempDirectory("edunussystestresults")
    tmpBinCacheDir = Files.createTempDirectory("edunussystestbincache")

    repo.clone(tmpRepoDir, Some(KnownGoodCommit))

    // Replace the expected output of "Valid." with anything else.
    // This works for this file, for this revision.
    val ReplaceCmd = Seq("sed", "-i", "s/ Valid\\. / Gibberish\\. /", "sleekengine.ml")
    val sedProc = Process(ReplaceCmd, tmpRepoDir.toFile)
    val sedExecOutp = Runnable.executeProc(sedProc)
 
    println("Running sed. Okay? " + (sedExecOutp.exitValue == 0))

    assume(sedExecOutp.exitValue == 0)
  }

  after {
    // delete the archive
    FileSystemUtilities rmdir tmpRepoDir
    FileSystemUtilities rmdir tmpResultsDir
    FileSystemUtilities rmdir tmpBinCacheDir
  }

  // We know the system works for 'dirty' repos if it gets different results
  // for a test after a commit is made.
  val KnownGoodTestCase = new TestCaseBuilder(Paths.get("sleek"), Paths.get("sleek.slk"), "", "Valid, Valid, Valid, Fail")


  "Dirty Repositories" should "be build in place; not " taggedAs(SlowTest) in {
    // Load the config, using `tmpRepoDir` instead of config's `REPO_DIR`.
    val tmpConfig = AppConfig.load(configuration, Some(tmpRepoDir))

    // Replace the bin cache, results dir in the config.
    println("DirtyRepoTest: binCacheDir is " + tmpBinCacheDir.toString)
    println("DirtyRepoTest: resultsDir is " + tmpResultsDir.toString)
    val TestConfig = tmpConfig.copy(resultsDir = tmpResultsDir.toString,
                                    binCacheDir = tmpBinCacheDir.toString)

    // Run some test on the repo;
    val repo = new Repository(tmpRepoDir)
    val runHipSleek = new RunHipSleek(TestConfig)

    val repoCommit = repo.identify()
    assert(repoCommit.isDirty)

    // Run the test
    val tsr = runHipSleek.altRunTests(SleekTestCase.constructTestCase, "sleek", List(KnownGoodTestCase))(repoCommit)
    val results = tsr.results
    assert(!results.isEmpty)
    val tcr = results.head

    // Since the "Valid." output was replaced,
    // only "Fail." will be caught from the actual `sleek` binary.
    // This yields an "Invalid" test.
    assert(!tcr.executionSucceeded)
    assert(!tcr.passed)


    // check that nothing was added to binCache;
    // (breaks abstraction to check dir, but oh well)
    assert(FileSystemUtilities isEmptyDirectory tmpBinCacheDir)

    // check that nothing was added to the resArch
    assert(FileSystemUtilities isEmptyDirectory tmpResultsDir)
  }
}
