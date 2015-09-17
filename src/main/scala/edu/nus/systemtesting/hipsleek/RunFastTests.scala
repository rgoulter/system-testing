package edu.nus.systemtesting.hipsleek

import scala.io.Source

/**
 * For parsing legacy `run-fast-tests.pl` (and similar) files.
 * It may be necessary to parse such files.
 */
object RunFastTests {
  type HipTestCase = (String, String, List[(String, String)])

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

  private def getHipFilesLines(rftLines: List[String]): List[String] = {
    // 1. accumulate lines between %hip_files=( ... );
    val StartRegex = "^%hip_files".r
    val fromLines = rftLines.dropWhile { line =>
      // drop while there's no regex match for the start
      (StartRegex findFirstIn line).isEmpty
    }

    // ends with );
    val EndRegex = "\\);$".r
    var hasMore = true
    val hipFilesLines = fromLines.takeWhile { line =>
      // Need to also take the line which has the );
      val next = hasMore
      hasMore = (EndRegex findFirstIn line).isEmpty
      next
    }

    hipFilesLines
  }

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

  private def processTestCaseStr(s: String): HipTestCase = {
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

  private def processFolderStr(s: String): (String, List[HipTestCase]) = {
    // s ~~ "folder" => [...]
    val parts = (s split "=>")
    assert(parts.length == 2)

    val folder = stripQuotes(parts(0))

    val TestCaseRegex = "\\[[^\\]]*\\]".r
    val rawTestCases = (TestCaseRegex findAllIn stripBrackets(parts(1))).toList

    // rawTestCases now contains list of e.g.
    //   ["append.ss",1, "--use-baga", "append","SUCCESS"]
    val testCases = rawTestCases map processTestCaseStr

    (folder, testCases)
  }

  def deriveHipTests(rftLines: List[String]): List[(String, List[HipTestCase])] = {
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
    val FolderRegex = "\"([^\"]*)\"\\s*=>\\s*\\[(\\[[^\\]]*\\],?)*\\]".r
    val rawFolders = (FolderRegex findAllIn inner).toList

    val folders = rawFolders map processFolderStr

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

    folders
  }


  def deriveSleekTests(rftLines: List[String]): Unit = {
    
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