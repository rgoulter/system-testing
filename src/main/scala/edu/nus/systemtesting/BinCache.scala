package edu.nus.systemtesting

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

/**
 * @author richardg
 */
class BinCache(val cacheDir: String = "bincache") {
  // ensure dir exists
  FileSystemUtilities.checkOutputDirectory(cacheDir)

  // store binaries under $cacheDir/<rev>/rel-to-proj-dir-path
  val cachePath = Paths.get(cacheDir)

  /**
   * @param cmd relative to bin/project dir; acts as a 'key'
   */
  def cache(binDir: Path, cmd: Path, revHash: String): Path = {
    // ensure $cacheDir/rev/path/to/bin exists.
    // ASSUME at the moment, that cmd directly in binDir..
    val dest = cachePath resolve revHash resolve cmd
    val destDir = dest getParent()
    FileSystemUtilities.checkOutputDirectory(destDir toString)

    assume(destDir.toFile().exists())

    // TODO: could warn if overwriting some file?
    Files.copy(binDir resolve cmd, dest)

    assume(dest.toFile().canExecute())

    dest
  }

  /**
   * @param cmd relative to project dir; acts as a 'key'
   * @param revHash needs to be the same as used by copy-to.
   */
  def binFor(cmd: Path, revHash: String): Option[Path] = {
    val dest = cachePath resolve revHash resolve cmd
    val exe = dest toFile()

    if (exe.exists() && exe.canExecute()) {
      Some(dest)
    } else {
      None
    }
  }
}