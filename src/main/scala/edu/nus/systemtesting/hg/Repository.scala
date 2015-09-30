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
  def archive(dest: Path, rev: Option[String], includePatterns: List[String] = List()): Boolean = {
    val inclArgs = includePatterns.map("-I " + _ + " ").mkString
    val cmd = "hg archive " + inclArgs + s"${rev.map(r => s" -r $r").getOrElse("")} ${dest.toString()}"
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

  def branchOf(rev: Option[String] = None): String = {
    val cmd = "hg identify -b" + rev.map(r => s" -r $r").getOrElse("")
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

  def commonAncestor(rev1: String, rev2: String): String = {
    // check rev1, rev2 are actually in the repo. These throw ex. if not
    require(identify(Some(rev1)) != "")
    require(identify(Some(rev2)) != "")

    // I'm not sure what happens if e.g. rev1, rev2 not related
    val cmd = s"hg debugancestor $rev1 $rev2"
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue == 0) {
      // output of debugancestor is like:
      //   16470:45c49c7e0321316504d10f1202aa0f58cfddd840
      // can either pass to identify(), or just trim + take 2nd.
      val res = execOutp.output.trim()
      res.split(":")(1)
    } else {
      throw new IllegalStateException
    }
  }

  /**
   * Returns a List of all the commits in the range `rev1...rev2`,
   * on the same branch.
   *
   * Assumes rev1, rev2 both valid revisions, and one is ancestor of the other,
   * and must be on the same branch.
   */
  def commitsInRange(rev1: String, rev2: String): List[String] = {
    // check rev1, rev2 are actually in the repo. These throw ex. if not
    require(identify(Some(rev1)) != "")
    require(identify(Some(rev2)) != "")

    val oldest = commonAncestor(rev1, rev2)
    val newest = if (oldest startsWith rev1) {
      rev2
    } else if (oldest startsWith rev2) {
      rev1
    } else {
      throw new IllegalArgumentException(s"Revisions must be related in linear manner")
    }

    val branch1 = branchOf(Some(rev1))
    val branch2 = branchOf(Some(rev2))

    require(branch1 == branch2)

    // I'm not sure what happens if e.g. rev1, rev2 not related
    val cmd = s"""hg log --template={node|short}\\n -r $oldest:$newest -b $branch1"""
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue == 0)
      execOutp.output.trim().lines.toList
    else
      throw new IllegalStateException
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

/** For debugging purposes */
private object RepositoryApp extends App {
  // range of commits 45c49c:79da9 should be like:
  //   45/40/c6/24/cc/ad/7a/4b/ce/80/79
//  val rev1 = "45c49c"
//  val rev2 = "79da9"
  //   01/f6/0b/c6/d1/89/0c/f5/fd/ff/a5/3b/f5
  val rev1 = "0111c"
  val rev2 = "f59ebb"

  // Hardcoded, but that's fine.
  import java.nio.file.Paths
  val repo = new Repository(Paths.get("/home/richardg/hg/sleekex"))

  val range = repo.commitsInRange(rev1, rev2)
  println(range)
  println(range map { s => s.substring(0, 2) } mkString("/"))
}