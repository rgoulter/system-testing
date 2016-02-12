package edu.nus.systemtesting.hipsleek.app

import java.io.File
import java.nio.file.Path

import org.scalatest.FlatSpec
import scala.sys.process._

import edu.nus.systemtesting.BinCache
import edu.nus.systemtesting.Runnable
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.hipsleek.BuildResult
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult

class RunUtilsSpec extends FlatSpec {
  def fakePrepare(projDir: Path): (BuildResult[Unit], Iterable[String]) =
    (SuccessfulBuildResult(()), List())

  "Run Utils" should "use separate folder for clean repository" in {
    // Need a "clean" HG repository.
    // Create this in a tmp dir.
    RunUtils.withTmpDir("edunusspechg") { tmpPath =>
      val tmpDir = tmpPath.toFile()
      val filename = "foo"
      val tmpFile = tmpDir.toPath().resolve(filename).toFile()
      tmpFile.createNewFile()

      Process("hg init", tmpDir).!
      (Process("echo A", tmpDir) #> tmpFile).!
      Process(s"hg add $filename", tmpDir).!
      Process("hg commit -m msg", tmpDir).!

      val repo = new Repository(tmpPath)
      val cmt = repo.identify()

      assert(!cmt.isDirty)

      // check that runWith uses run-with folder.
      val fakeBinCache = new BinCache("fake") {
        override def cache(binDir: Path, cmd: Path, rev: Commit): Option[Path] = None
        override def cachedDirFor(rev: Commit): Option[Path] = None
      }

      // Can't override run-with-repo, etc. as these are private.
      // So, just check that the paths aren't the same.
      val runUt = new RunUtils(tmpPath, fakeBinCache, filesToCopy = List(), prepare = fakePrepare)

      runUt.runTestsWith(cmt, List()) { case (binDir, corpusDir, rev) =>
        assert(tmpPath != binDir)
        assert(tmpPath != corpusDir)
      }
    }
  }

  it should "use in-place for dirty repository" in {
    // need a "dirty" repository.
    // Create this in a tmp dir.
    RunUtils.withTmpDir("edunusspechg") { tmpPath =>
      val tmpDir = tmpPath.toFile()
      val filename = "foo"
      val tmpFile = tmpDir.toPath().resolve(filename).toFile()
      tmpFile.createNewFile()

      Process("hg init", tmpDir).!
      (Process("echo A", tmpDir) #> tmpFile).!
      Process(s"hg add $filename", tmpDir).!
      Process("hg commit -m msg", tmpDir).!

      // Make it dirty
      (Process("echo B", tmpDir) #>> tmpFile).!

      val repo = new Repository(tmpPath)
      val cmt = repo.identify()

      assert(cmt.isDirty)

      // check that runWith uses run-with folder.
      val fakeBinCache = new BinCache("fake") {
        override def cache(binDir: Path, cmd: Path, rev: Commit): Option[Path] = None
        override def cachedDirFor(rev: Commit): Option[Path] = None
      }

      // Can't override run-with-repo, etc. as these are private.
      // So, just check that the paths are the same.
      val runUt = new RunUtils(tmpPath, fakeBinCache, filesToCopy = List(), prepare = fakePrepare)

      runUt.runTestsWith(cmt, List()) { case (binDir, corpusDir, rev) =>
        // Dirty, so should be run in-place
        assert(tmpPath == binDir)
        assert(tmpPath == corpusDir)
      }
    }
  }

  it should "delete the tmpdir after use" in {
    val tmpDir = RunUtils.withTmpDir("edunusspec") { tmpPath =>
      val tmpDir = tmpPath.toFile()

      assert(tmpDir.exists())

      tmpDir
    }

    // Should be deleted by now.
    assert(!tmpDir.exists())
  }

  it should "delete the tmpdir after use even if throws exception" in {
    var testTmpDir: File = null

    intercept[RuntimeException] {
      RunUtils.withTmpDir("edunusspec") { tmpPath =>
        testTmpDir = tmpPath.toFile()

        assert(testTmpDir.exists())

        throw new RuntimeException()
      }
    }

    // Should be deleted by now.
    assert(!testTmpDir.exists())
  }
}