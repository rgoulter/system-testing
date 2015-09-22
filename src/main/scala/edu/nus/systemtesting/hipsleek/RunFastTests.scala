package edu.nus.systemtesting.hipsleek

import scala.io.Source
import scala.util.matching.Regex

/**
 * For parsing legacy `run-fast-tests.pl` (and similar) files.
 * It may be necessary to parse such files.
 */
object RunFastTests {
  type HipTest = (String, String, List[(String, String)])
  type SleekTest = (String, String, List[String])

  // General structure of the info we want in run-fast-test:
  //
  // Hip:
  //   %hip_files=(
  //               "name"=>[["filename", #expected, "--args", "proc1", "exp1",...],...]
  //               ...
  //              );
  //
  // Sleek:
  //   %sleek_files=(
  //                 "name"=>[["filename", "--args", ([$var,"Exp."]), "Exp1.Exp2."],...]
  //                 ...
  //                 );
  //
  // * The %hip_files= will start at the beginning of the line
  //   * No other `);` until the end of hip_files.
  // * => *can* be separated by a space.
  // * Mix of tabs, spaces (not just leading tabs) for whitespace.
  // * Hip expected is entirely SUCCESS, FAIL
  // * Sleek is mostly Valid, Fail,
  //   * except for `musterr` folder, which has must, may.
  //     (and in older versions, even 'bot')
  //   * cf. w/ run-fast-tests functionality, but EXC is possible, too?

  private def getFileLines(start: Regex, end: Regex)(rftLines: List[String]): List[String] = {
    // 1. accumulate lines between %hip_files=( ... );
    val fromLines = rftLines.dropWhile { line =>
      // drop while there's no regex match for the start
      (start findFirstIn line).isEmpty
    }

    // ends with );
    var hasMore = true
    fromLines.takeWhile { line =>
      // Need to also take the line which has the );
      val next = hasMore
      hasMore = (end findFirstIn line).isEmpty
      next
    }
  }

  private def getHipFilesLines: List[String] => List[String] =
    getFileLines("^%hip_files".r,
                 "\\);$".r)(_)

  private def getSleekFilesLines: List[String] => List[String] =
    getFileLines("^%sleek_files".r,
                 "\\);$".r)(_)


  /**
   * Trim lines,
   * take out the ones which are comments
   */
  private def tidyPerlLines(lines: List[String]): List[String] = {
    // Recall:
    // abc # def --split-> [abc, def]
    // abc       --split-> [abc]
    // # def     --split-> ["", def]
    def beforeComment(line: String): String = (line split "#")(0)

    lines.map(beforeComment _).map(_.trim()).filterNot(_.isEmpty())
  }

  private def stripOutermost(open: String, close: String)(s: String): String = {
    val firstIdx = s indexOf open
    val lastIdx  = s lastIndexOf close

    assert(firstIdx >= 0, s"first $open for $s")
    assert(lastIdx >= 0, s"last $close for $s")

    s.substring(firstIdx + 1, lastIdx)
  }

  private def stripParentheses: String => String =
    stripOutermost("(", ")")(_)

  private def stripBrackets: String => String =
    stripOutermost("[", "]")(_)

  private def stripQuotes: String => String =
    stripOutermost("\"", "\"")(_)

  private def processHipTestCaseStr(s: String): HipTest = {
    // s ~~ ["append.ss",1, "--use-baga", "append","SUCCESS"]
    val inner = stripBrackets(s)
    val parts = inner.split(",")

    val file = stripQuotes(parts(0))
    val args = stripQuotes(parts(2))

    // rawExpected = "proc1", "expect1", ...
    // (Need to filter for not empty, because some are malformed,
    //  e.g. bags/avl-all.ss)
    var rawExpected = parts.drop(3).map(_.trim()).filterNot(_.isEmpty()).toList
    var expected: List[(String, String)] = List()

    // build expected from rawExpected.
    while (rawExpected.length >= 2) {
      val proc = stripQuotes(rawExpected(0))
      val expect = stripQuotes(rawExpected(1))
      expected = expected :+ (proc, expect)
      rawExpected = rawExpected.drop(2)
    }

    if (!rawExpected.isEmpty) {
      System.err.println(s"Warning! Malformed TestCase:\n$s")
    }

    (file, args, expected)
  }

  // n.b. From run-fast-tests.pl:
  //   $lem = '--elp';
  //   $inv = '--inv-test';
  //   $dis = '--dis-inv-baga';

  // regex didn't like
  // ["../tree_shares/barrier.slk", "--eps --dis-field-imm --dis-precise-xpure -perm dperm", "Barrrier b1n Success.Barrrier b3n Fail:  frames do not match (1->2).Barrrier b2n Fail:  contradiction in post for transition (1->2).Barrrier b4n Fail:  no contradiction found in preconditions of transitions from 1  for preconditions: .", ""]
  // ["../tree_shares/barrier.slk",
  //  "--eps --dis-field-imm --dis-precise-xpure -perm dperm",
  //  "Barrrier b1n Success.Barrrier b3n Fail:  frames do not match (1->2).Barrrier b2n Fail:  contradiction in post for transition (1->2).Barrrier b4n Fail:  no contradiction found in preconditions of transitions from 1  for preconditions: .",
  //  ""]

  val SleekTestRegex = "\"([^\"]*)\",\\s*\"([^\"]*)\",\\s*\\(([^\\)]*)\\),\\s*\"([^\"]*)\"\\s*"
  val SleekTestAltRegex = "\"([^\"]*)\",\\s*\"([^\"]*)\",\\s*\"[^\"]*\",\\s*\"([^\"]*)\"\\s*"
  
  private def processSleekTestCaseStr(s: String): SleekTest = {
    // s ~~ ["lst-under2.slk", "--inv-test", ([$dis,"Fail.Valid"]), "Valid.Fail."],
//    val inner = stripBrackets(s)

    // ... and some don't follow this,
    // e.g. threads/thrd1.slk & search for `Barrrier`

    s match {
      case SleekTestRegex.r(filename, args, para, rawExpected) => {
        // What's happing with the `para` bit?
        // Just prepend expected with the 'lemma expected' or so.

        // e.g. ([$lem,"Fail.Valid."]), => para = [$lem,"Fail.Valid."]
        val lemmaRawExpected =
          (("\".*\"".r findFirstIn para) map stripQuotes).getOrElse("")

        val expected =
          (lemmaRawExpected + rawExpected).split("\\.").filterNot(_.isEmpty()).toList
        (filename, args, expected)
      }

      case SleekTestAltRegex.r(filename, args, rawExpected) => {
        // what's happing with the `para` bit?
        val expected = rawExpected.split("\\.").filterNot(_.isEmpty()).toList
        (filename, args, expected)
      }

      case _ => throw new RuntimeException(s"Bad line: $s")
    }
  }

  private def processFolderStr[T](s: String, testCaseRegex: Regex, procTestCaseStr: String => T): (String, List[T]) = {
    // s ~~ "folder" => [...]
    val parts = (s split "=>")
    assert(parts.length == 2)

    val folder = stripQuotes(parts(0))

//    val TestCaseRegex = "\\[[^\\]]*\\]".r
    val rawTestCases = (testCaseRegex findAllIn stripBrackets(parts(1))).toList

    // rawTestCases now contains list of e.g.
    //   ["append.ss",1, "--use-baga", "append","SUCCESS"]
    val testCases = rawTestCases map procTestCaseStr

    (folder, testCases)
  }

  private def processHipFolderStr(s: String): (String, List[HipTest]) = {
    val TestCaseRegex = "\\[[^\\]]*\\]".r
    processFolderStr[HipTest](s, TestCaseRegex, processHipTestCaseStr)
  }

  private def processSleekFolderStr(s: String): (String, List[SleekTest]) = {
    processFolderStr[SleekTest](s, s"$SleekTestRegex|$SleekTestAltRegex".r, processSleekTestCaseStr)
  }

  def deriveHipTests(rftLines: List[String]): List[(String, List[HipTest])] = {
    val hipFilesLines = tidyPerlLines(getHipFilesLines(rftLines))

    // Now make one huge string of it
    val hipFilesStr = hipFilesLines.mkString
    val inner = stripParentheses(hipFilesStr)

    // find each folder line
    // A "write once" regex:
    //   \"([^\"]*)\"\\s*=>\\s*\\[(\\[[^\\]]*\\],?)*\\]
    // Match "folder" => [...]
    //   \"([^\"]*)\"\\s*=>\\s*\\[(               )*\\]
    // Match the testcases inside
    //                            (\\[[^\\]]*\\],?)*
    val FolderRegex = "\"([^\"]*)\"\\s*=>\\s*\\[\\s*(\\[[^\\]]*\\],?\\s*)*\\]".r
    val rawFolders = (FolderRegex findAllIn inner).toList

    val folders = rawFolders map processHipFolderStr

    folders foreach { folder =>
      val (name, testcases) = folder
      println(s"Folder $name")

      testcases foreach { testcase =>
        val (name, args, expects) =  testcase
        val expectsStr = expects map { case (p,e) => s"$p: $e" } mkString ", "
        println(s"$name $args $expectsStr")
      }

      println
    }

    folders foreach { folder =>
      val (name, testcases) = folder
      println(s"Folder $name")
    }

    folders
  }

  def deriveSleekTests(rftLines: List[String]): List[(String, List[SleekTest])] = {
    val sleekFilesLines = tidyPerlLines(getSleekFilesLines(rftLines))

    // Now make one huge string of it
    val sleekFilesStr = sleekFilesLines.mkString
    val inner = stripParentheses(sleekFilesStr)

    // find each folder line
    // A "write once" regex:
    //   \"([^\"]*)\"\\s*=>\\s*\\[(\\[[^\\]]*\\],?)*\\]
    // Match "folder" => [...]
    //   \"([^\"]*)\"\\s*=>\\s*\\[(               )*\\]
    // Match the testcases inside
    //                            (\\[[^\\]]*\\],?)*
    val InnerRegex = s"(?:$SleekTestRegex|$SleekTestAltRegex)"
    val FolderRegex = ("\"([^\"]*)\"\\s*=>\\s*\\[\\s*(\\[" + InnerRegex + "\\],?\\s*)*\\]").r
    val rawFolders = (FolderRegex findAllIn inner).toList

    val folders = rawFolders map processSleekFolderStr

    folders foreach { folder =>
      val (name, testcases) = folder
      println(s"Folder $name")

      testcases foreach { testcase =>
        val (name, args, expects) =  testcase
        println(s"$name $args $expects")
      }

      println
    }

    folders foreach { folder =>
      val (name, testcases) = folder

      println(s"Folder $name")
    }

    folders
  }


  // Sleek TestCase isn't so clear what the () is for.
  // e.g. line 1958 @ 79da
  // ["lemmas/ll_tail.slk", " --elp ", ([$lem,"Valid.Valid"]), "Valid.Valid"],

  // Regarding other perl files in the repo,
  // * run-medium-tests seems to be of similar format, but much shorter,
  // * run-long-tests is something quite different.

  def main(args: Array[String]): Unit = {
    // Hardcoded here, but not something we need to set often, so.
    val runFastTestsFilename = "/home/richardg/hg/sleekex/examples/working/run-fast-tests.pl"

    val rftSrc = Source.fromFile(runFastTestsFilename)
    val rftLines = rftSrc.getLines().toList
    rftSrc.close()

    deriveHipTests(rftLines)

    deriveSleekTests(rftLines)
  }
}