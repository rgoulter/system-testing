package edu.nus.systemtesting

import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterAll
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import org.scalatest.ConfigMap
import edu.nus.systemtesting.hg.Repository
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfter
import edu.nus.systemtesting.hipsleek.BuildFailed
import edu.nus.systemtesting.hipsleek.HipSleekPreparation
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
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
    // export an archive, so we can check our system prep works
    tmpRepoDir = Files.createTempDirectory("edunussystestrepo")
    tmpResultsDir = Files.createTempDirectory("edunussystestresults")
    tmpBinCacheDir = Files.createTempDirectory("edunussystestbincache")

    repo.clone(tmpRepoDir, Some(KnownGoodCommit))

    // Replace the expected output of "Valid." with anything else.
    // This works for this file, for this revision.
    val ReplaceCmd = Seq("sed", "-i", "s/ Valid\\. / Gibberish\\. /", "sleekengine.ml")
    // XXX run this cmd in tmpRepoDir
  }

  after {
    // delete the archive
    val tmpDir = tmpRepoDir.toFile()
    tmpDir.delete()
  }

  // We know the system works for 'dirty' repos if it gets different results
  // for a test after a commit is made.
  val KnownGoodTestCase = new TestCaseBuilder(Paths.get("sleek"), Paths.get("sleek.slk"), "", "Valid, Valid, Valid, Fail")

  // XXX run some test on the repo;
  // XXX check that nothing was added to binCache;
  // XXX check that nothing was added to the resArch


  "Dirty Repositories" should "make a valid repo" taggedAs(SlowTest) in {
    val prep = new HipSleekPreparation(tmpRepoDir)

    val (res, remarks) = prep.prepare()

    assert(res.isInstanceOf[SuccessfulBuildResult[Unit]])
  }
}
