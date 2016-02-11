package edu.nus.systemtesting

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import org.scalatest.BeforeAndAfter
import org.scalatest.FlatSpec

import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigFactory

import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.hipsleek.BuildFailed
import edu.nus.systemtesting.hipsleek.HipSleekPreparation
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult

/**
 * @author richardg
 */
class SystemPrepSpec extends FlatSpec with BeforeAndAfter {
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

  var tmpArchiveDir: Path = _

  before {
    // export an archive, so we can check our system prep works
    tmpArchiveDir = Files.createTempDirectory("edunussystest")

    // constructing an archive takes ~10s
    // though, building the ML code takes much more time, so.
    repo.archive(tmpArchiveDir, KnownGoodCommit)
  }

  after {
    // delete the archive
    val tmpDir = tmpArchiveDir.toFile()
    tmpDir.delete()
  }

  "Hip/Sleek system prep" should "make a valid repo" taggedAs(SlowTest) in {
    val prep = new HipSleekPreparation(tmpArchiveDir)

    val (res, remarks) = prep.prepare()

    assert(res.isInstanceOf[SuccessfulBuildResult[Unit]])
  }

  it should "detect when make failed" taggedAs(SlowTest) in {
    val prep = new HipSleekPreparation(tmpArchiveDir)

    // Need to break something in the build.
    // remove main.ml would do it.
    val mainMl = tmpArchiveDir.resolve("main.ml")
    val mainMlFile = mainMl.toFile()

    val deleted = mainMlFile.delete()
    assume(deleted)

    val (res, remarks) = prep.prepare()

    assert(res.isInstanceOf[BuildFailed[Unit]])
  }
}
