package edu.nus.systemtesting

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.sys.process.Process
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.ConfigMap
import org.scalatest.BeforeAndAfter
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.hipsleek.BuildFailed
import edu.nus.systemtesting.hipsleek.HipSleekPreparation
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.hipsleek.SleekTestCase
import edu.nus.systemtesting.hipsleek.app.AppConfig
import edu.nus.systemtesting.hipsleek.app.RunHipSleek
import com.typesafe.config.ConfigException
import java.io.PrintWriter


/**
 * @author richardg
 */
class ResultArchiveUsageSpec extends FlatSpec with BeforeAndAfter {
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

  // Phrase must be in the remarks of a TestCaseResult which is loaded from file
  val MagicPhrase = "ResultLoadedFromFileForTest"

  var tmpRepoDir: Path = _
  var tmpResultsDir: Path = _
  var tmpBinCacheDir: Path = _
  
  before {
    // Clone the repo
    tmpRepoDir = Files.createTempDirectory("edunussystestrepo")
    tmpResultsDir = Files.createTempDirectory("edunussystestresults")
    tmpBinCacheDir = Files.createTempDirectory("edunussystestbincache")


    val repo = new Repository(REPO_DIR)
    val KnownGoodCommit = repo.identify(Some(KnownGoodRevision))

    repo.clone(tmpRepoDir, Some(KnownGoodCommit))

    // The easiest way to tell whether a result is used is to have a booby JSON result,
    // with a magic phrase which doesn't occur ...
    val JSONForInvalidTestCase = s"""{
  "command" : "sleek",
  "executionTime" : "77",
  "filename" : "examples/working/sleek/sleek.slk",
  "remarks" : [
    "$MagicPhrase"
  ],
  "arguments" : ""
}"""
    // Dump this to tmpResultsDir/KnownGoodRevision/sleek_examplesworkingsleeksleekslk_.json
    val revFolder = (tmpResultsDir resolve KnownGoodRevision)
    assume(revFolder.toFile().mkdir())

    val resultFilepath = revFolder resolve "sleek_examplesworkingsleeksleekslk_.json"
    val out = new PrintWriter(resultFilepath.toFile())
    out.println(JSONForInvalidTestCase)
    out.flush()
    out.close()
  }

  after {
    // delete the archive
    FileSystemUtilities rmdir tmpRepoDir
    FileSystemUtilities rmdir tmpResultsDir
    FileSystemUtilities rmdir tmpBinCacheDir
  }

  // We know the system works for 'dirty' repos if it gets different results
  // for a test after a commit is made.
  val KnownGoodTestCase = new TestCaseBuilder(Paths.get("sleek"), Paths.get("examples/working/sleek/sleek.slk"), "", "Valid, Valid, Valid, Fail")

  "Results from file" should "be used when results are there" taggedAs(SlowTest) in {
    // Load the config, using `tmpRepoDir` instead of config's `REPO_DIR`.
    val tmpConfig = AppConfig.load(configuration, Some(tmpRepoDir))
    val TestConfig = tmpConfig.copy(resultsDir = tmpResultsDir.toString,
                                    binCacheDir = tmpBinCacheDir.toString)
    // Run some test on the repo;
    val repo = new Repository(tmpRepoDir)
    val runHipSleek = new RunHipSleek(TestConfig)

    val repoCommit = repo.identify()

    // Run the test
    val tsr = runHipSleek.altRunTests(SleekTestCase.constructTestCase, List(KnownGoodTestCase))(repoCommit)
    val results = tsr.results
    assert(!results.isEmpty)
    val tcr = results.head

    // Assert that the result was used
    // remarks will contain MagicPhrase iff TCR loaded from the JSON.
    assert(!tcr.remarks.isEmpty)
    assert(tcr.remarks contains MagicPhrase)
  }

  it should "NOT be used when dirty" taggedAs(SlowTest) in {
    // Load the config, using `tmpRepoDir` instead of config's `REPO_DIR`.
    val tmpConfig = AppConfig.load(configuration, Some(tmpRepoDir))
    val TestConfig = tmpConfig.copy(resultsDir = tmpResultsDir.toString,
                                    binCacheDir = tmpBinCacheDir.toString)

    // Make some change to the Repo, to make it dirty
    // Replace the expected output of "Valid." with anything else.
    // This works for this file, for this revision.
    val ReplaceCmd = Seq("sed", "-i", "s/frontend/gibberish/", "sleekengine.ml")
    val sedProc = Process(ReplaceCmd, tmpRepoDir.toFile)
    val sedExecOutp = Runnable.executeProc(sedProc)

    assume(sedExecOutp.exitValue == 0)

    // Run some test on the repo;
    val repo = new Repository(tmpRepoDir)
    val runHipSleek = new RunHipSleek(TestConfig)

    val repoCommit = repo.identify()
    assert(repoCommit.isDirty)

    // Run the test
    val tsr = runHipSleek.altRunTests(SleekTestCase.constructTestCase, List(KnownGoodTestCase))(repoCommit)
    val results = tsr.results
    assert(!results.isEmpty)
    val tcr = results.head

    // Assert that the result was used
    // remarks will contain MagicPhrase iff TCR loaded from the JSON.
    assert(tcr.passed)
    assert(!(tcr.remarks contains MagicPhrase))
  }
}