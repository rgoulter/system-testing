package edu.nus.systemtesting.hg

import scala.sys.process.Process
import java.io.File
import edu.nus.systemtesting.Runnable
import java.nio.file.Path

class UnknownRevisionException(badRev: String, repoDir: Path)
    extends IllegalArgumentException(s"Bad revision: $badRev in HG repo $repoDir")

/**
 * For modelling non-destructive mercurial commands on a repository.
 * @author richardg
 */
class Repository(dir: Path) {
  require((dir resolve ".hg") toFile() exists, "dir must be an HG repository")

  val repoDir = dir toFile

  /**
   * Creates a copy of some revision of the repo at some `dest`.
   * (Latest revision if `rev` is `None`).
   */
  def archive(dest: Path, rev: Option[String]): Boolean = {
    val cmd = "hg archive" + s"${rev.map(r => s" -r $r").getOrElse("")} ${dest.toString()}"
    val proc = Process(cmd, repoDir)

    println(s"Exporting archive of $dir to $dest " +
            s"(${rev.map(r => "Revision " + r).getOrElse("Current revision")})")
    val execOutp = Runnable.executeProc(proc)

    // for now, let's not deal with errors
    execOutp.exitValue != 0
  }

  def identify(rev: Option[String] = None): String = {
    val cmd = "hg identify -i" + rev.map(r => s" -r $r").getOrElse("")
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue == 0)
      execOutp.output.trim()
    else
      throw new UnknownRevisionException(rev.getOrElse("<head>"), dir)
  }

  /**
   * Returns list of (short) revision hashes of the parent(s) of the given
   * revision.
   *
   * Will mostly return list length one, except for merge commits.
   */
  def parents(rev: Option[String] = None): List[String] = {
    val cmd = "hg parents --template {node|short}" + rev.map(r => s" -r $r").getOrElse("")
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue == 0)
      execOutp.output.trim().lines.toList
    else
      throw new UnknownRevisionException(rev.getOrElse("<head>"), dir)
  }

  def isDirty(): Boolean = {
    val hash = identify()
    hash.endsWith("+")
  }

  def status(): Iterable[(String, String)] = {
    /* Recall, from `hg help status`
     M = modified
     A = added
     R = removed
     C = clean
     ! = missing (deleted by non-hg command, but still tracked)
     ? = not tracked
     I = ignored
       = origin of the previous file listed as A (added)
     */

    val cmd = s"hg status"
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    // for now, let's not deal with errors
    execOutp.stdoutLines.map(line => {
      // line is like
      //   "? file.txt"
      val parts = line.split(" ", 2)
      (parts(0), parts(1))
    })
  }
}
