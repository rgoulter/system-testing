package edu.nus.systemtesting.hg

import scala.sys.process.Process
import java.io.File
import edu.nus.systemtesting.Runnable
import java.nio.file.Path
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.ISODateTimeFormat

class UnknownRevisionException(badRev: String, repoDir: Path)
    extends IllegalArgumentException(s"Bad revision: $badRev in HG repo $repoDir")


class Commit(repo: Repository, rev: String) {
  val HashLength = 12

  require(rev.length >= HashLength)

  val revHash = rev.substring(0, HashLength)
  val isDirty = rev.last == '+'

  lazy val age: String =
    repo.logForTemplate("{date|age}\\n", revHash)

  lazy val date = {
    val isoStr = repo.logForTemplate("{date|isodate}\\n", revHash)
    Repository.parseHgIsodate(isoStr)
  }

  lazy val branch =
    new Branch(repo, repo.logForTemplate("{branch}\\n", revHash))

  override def toString() = revHash
}


object Branch {
  implicit def branchToCommit(b: Branch): Commit = b.latestCommit
}


class Branch(repo: Repository, val name: String) {
  lazy val latestCommit: Commit =
    // Unintuitively, with reverse() this gets the latest.
    // hg log -r "limit(reverse(branch($BRANCH)), 1)"
    new Commit(repo, repo.logForTemplate("{node|short}\\n", s"limit(reverse(branch('$name')), 1)"))

  lazy val earliestCommit: Commit =
    // hg log -r "limit((branch($BRANCH)), 1)"
    new Commit(repo, repo.logForTemplate("{node|short}\\n", s"limit(branch('$name'), 1)"))

  // may not have a parent branch
  lazy val branchedFrom: Option[Commit] =
    repo.parents(earliestCommit).headOption

  override def toString() = name
}


object Repository {
  def parseHgIsodate(dateStr : String): DateTime = {
    // Using `isodate` filter, we get a datetime string like:
    //   2015-09-23 14:07 +0800
    // This can be matched in Joda using format:
    //   yyyy-MM-DD HH:mm Z
    // Note that may need to be careful about timezones, to be precise:
    // cf. http://stackoverflow.com/questions/16794772/joda-time-parse-a-date-with-timezone-and-retain-that-timezone#16796199
    val df = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm Z");
    df.withOffsetParsed().parseDateTime(dateStr);
  }
}

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
  def archive(dest: Path, revision: Commit, includePatterns: List[String] = List()): Boolean = {
    val rev = Some(revision.revHash) // XXX: Can simplify

    val inclArgs = includePatterns.map("-I " + _ + " ").mkString
    val cmd = "hg archive " + inclArgs + s"${rev.map(r => s" -r $r").getOrElse("")} ${dest.toString()}"
    val proc = Process(cmd, repoDir)

    println(s"Exporting archive of $dir to $dest " +
            s"(${rev.map(r => "Revision " + r).getOrElse("Current revision")})")
    val execOutp = Runnable.executeProc(proc)

    // for now, let's not deal with errors
    execOutp.exitValue != 0
  }

  /** Whether the given `rev` evaluaties to a revision hash in the repo. */
  def isRevisionish(rev: String): Boolean = {
    require(rev != "")

    val cmd = s"hg identify -i -r $rev"
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    execOutp.exitValue == 0
  }

  /**
   * Return `Commit` instance of the given rev, or throw `UnknownRevisionException`
   * if the argument doesn't resolve to a commit.
   *
   * If `rev` is `None`, then the revision may be dirty.
   */
  def identify(rev: Option[String] = None): Commit = {
    val cmd = "hg identify -i" + rev.map(r => s" -r $r").getOrElse("")
    val proc = Process(cmd, repoDir)

    // Output of `hg id -i` is 12-chars of the hash,
    // additionally, a `+` if the rev is `dirty`.

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue == 0)
      new Commit(this, execOutp.output.trim())
    else
      throw new UnknownRevisionException(rev.getOrElse("<head>"), dir)
  }

  /**
   * Returns list of (short) revision hashes of the parent(s) of the given
   * revision.
   *
   * Will mostly return list length one, except for merge commits.
   */
  def parents(revision: Commit): List[Commit] = {
    val rev = if (revision.isDirty) None else Some(revision.revHash)

    val cmd = "hg parents --template {node|short}" + rev.map(r => s" -r $r").getOrElse("")
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue == 0)
      execOutp.output.trim().lines.map(new Commit(this, _)).toList
    else
      throw new UnknownRevisionException(rev.getOrElse("<head>"), dir)
  }

  def commonAncestor(rev1: Commit, rev2: Commit): Commit = {
    // I'm not sure what happens if e.g. rev1, rev2 not related
    val cmd = s"hg debugancestor $rev1 $rev2"
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue == 0) {
      // output of debugancestor is like:
      //   16470:45c49c7e0321316504d10f1202aa0f58cfddd840
      // can either pass to identify(), or just trim + take 2nd.
      val res = execOutp.output.trim()

      new Commit(this, res.split(":")(1))
    } else {
      throw new IllegalStateException
    }
  }

  def logForTemplate(template: String, revStr: String, additionalArgs: String*): String = {
    val cmd = Seq("hg", "log", s"--template=$template", "-r", revStr) ++ additionalArgs
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue != 0) {
      println(s"### ERROR running command: ${cmd.mkString(" ")}")
      println(execOutp.errOutput)
    }
    assert(execOutp.exitValue == 0)

    execOutp.output.trim()
  }

  /**
   * Returns a List of all the commits in the range `rev1...rev2`,
   * on the same branch.
   *
   * Assumes rev1, rev2 both valid revisions, and one is ancestor of the other,
   * and must be on the same branch.
   */
  def commitsInRange(rev1: Commit, rev2: Commit): List[Commit] = {
    val oldest = commonAncestor(rev1, rev2)
    val newest = if (oldest.revHash startsWith rev1.revHash) {
      rev2
    } else if (oldest.revHash startsWith rev2.revHash) {
      rev1
    } else {
      throw new IllegalArgumentException(s"Revisions must be related in linear manner")
    }

    // from `hg help revsets`,
    //   "x::y"
    //     A DAG range, meaning all changesets that are descendants of x and
    //     ancestors of y, including x and y themselves. If the first endpoint is
    //     left out, this is equivalent to "ancestors(y)", if the second is left
    //     out it is equivalent to "descendants(x)".
    //
    //     An alternative syntax is "x..y".

    // TODO: Can simplify to use logForTemplate
    val cmd = s"""hg log --template={node|short}\\n -r $oldest::$newest"""
    val proc = Process(cmd, repoDir)

    val execOutp = Runnable.executeProc(proc)

    if (execOutp.exitValue == 0)
      execOutp.output.trim().lines.map(new Commit(this, _)).toList
    else
      throw new IllegalStateException
  }

  def isDirty(): Boolean = {
    // whether repo is dirty is property of commit, not of repo.
    identify().isDirty
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

  def heads(): List[Commit] = {
    val heads = logForTemplate("{node|short}\\n", "head()")
    heads.lines.toList.map(new Commit(this, _))
  }

  def tip(): Commit = {
    val tipHash = logForTemplate("{node|short}\\n", "tip")
    new Commit(this, tipHash)
  }

  def recentBranches(): List[Commit] = {
    val latestCommit = tip()
    val recentDate = latestCommit.date.minusDays(30)

    val fmt = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
    val dateStr = recentDate.toString(fmt)

    val rec = logForTemplate("{node|short}\\n", "head()", "--date", s">$dateStr")
    rec.lines.toList.map(new Commit(this, _))
  }
}

/** For debugging purposes */
private object RepositoryApp extends App {
  // Hardcoded, but that's fine.
  import java.nio.file.Paths
  val repo = new Repository(Paths.get("/home/richardg/hg/sleekex"))

  val t = repo.tip()
  val tdate = t.date

  val isoFmt = ISODateTimeFormat.dateTime()
  val dateStr = tdate.toString(isoFmt)
  println("Latest commit:" + dateStr)

  def branchInfo(b: Branch) {
    val bf = b.branchedFrom
    val bfStr = bf.map({ c => Seq(c.branch.name, c.revHash, c.age).mkString(" ") }).getOrElse("-")

    println(s"${b.name} ${b.latestCommit.revHash} ${b.age} branched from $bfStr")
  }

  println("### Recent Branches: ###")
  val recentBr = repo.recentBranches()
  recentBr.foreach { x =>
    branchInfo(x.branch)
  }
  println(recentBr.length + " many.")
}