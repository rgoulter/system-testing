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
import edu.nus.systemtesting.hipsleek.HipSleekPreparation

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
  val REPO_DIR = configuration.getString("REPO_DIR")
  val repo = new Repository(REPO_DIR)

  var tmpArchiveDir : Path = _

  before {
    // export an archive, so we can check our system prep works
    tmpArchiveDir = Files.createTempDirectory("edunussystest")

    // constructing an archive takes ~10s
    // though, building the ML code takes much more time, so.
    repo.archive(tmpArchiveDir.toAbsolutePath().toString(), Some(KnownGoodRevision))
  }

  after {
    // delete the archive
    val tmpDir = tmpArchiveDir.toFile()
    tmpDir.delete()
  }

  "Hip/Sleek system prep" should "make a valid repo" in {
    val prep = new HipSleekPreparation(tmpArchiveDir.toAbsolutePath().toString())

    val (res, remarks) = prep.prepare()

    assert(res)
  }

  it should "detect when make failed" in {
    val prep = new HipSleekPreparation(tmpArchiveDir.toAbsolutePath().toString())

    // Need to break something in the build.
    // remove main.ml would do it.
    val mainMl = tmpArchiveDir.resolve("main.ml")
    val mainMlFile = mainMl.toFile()

    val deleted = mainMlFile.delete()
    assume(deleted)

    val (res, remarks) = prep.prepare()

    assert(!res)
  }
}