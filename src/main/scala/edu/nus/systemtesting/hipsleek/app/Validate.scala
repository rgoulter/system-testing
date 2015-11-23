package edu.nus.systemtesting.hipsleek.app

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.output.GlobalReporter.reporter
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.hipsleek.BuildResult
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.PreparedSystem
import edu.nus.systemtesting.hipsleek.ValidateableSleekTestCase
import edu.nus.systemtesting.TestCaseBuilder
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.ExpectsOutput

/**
 * @author richardg
 */
class Validate(config: AppConfig) {
  val repoDirPath = config.repoDirOrDie
  val repoDir = repoDirPath.toFile

  // Each instance of `ConfiguredMain` only ever uses the one `Repository`
  val repo = new Repository(config.repoDirOrDie)


  val runUtils = new RunUtils(config)
  import runUtils.runTestsWith
  val runHipSleek = new RunHipSleek(config)
  import runHipSleek.{ altRunTests, runTestCaseForRevision }

  /** Based upon the `test/` dirname convention. */
  def listTestableDirs(): List[Path] = {
    // make use of unix commands: find, xargs, dirname, grep
    import scala.sys.process._
    val testDirs = Process(s"find $repoDirPath -mindepth 2 -type d -name test") #| "grep -v \\.hg" !!

    testDirs.split("\n").toList.map(repoDirPath.resolve)
  }

  def isValidateableFile(f: File): Boolean = {
    // Use `-q` flag so grep doesn't outputs nothing.
    import scala.sys.process.Process
    val exitStatus = Process(s"grep -q ^expect.*\\.$$ ${f.getAbsolutePath}") !

    exitStatus == 0
  }

  def listValidateableInDir(dirPath: Path): List[File] = {
    val dir = dirPath.toFile()
    require(dir.isDirectory())

    dir.listFiles().toList.filter(f => f.getName.endsWith(".slk") && f.isFile()).filter(isValidateableFile)
  }

  // no expected output needed,
  // assumes validateable should run without any arguments
  private def testableForFile(f: File): Testable with ExpectsOutput =
    (new TestCaseBuilder()
       runCommand Paths.get("sleek")
       onFile repoDirPath.relativize(f.toPath()))

  def allTestable: List[Testable with ExpectsOutput] =
    listTestableDirs() flatMap listValidateableInDir map testableForFile

  private[app] def runSleekValidateTests(rev: Commit): TestSuiteResult =
    altRunTests(ValidateableSleekTestCase.constructTestCase,
                allTestable)(rev)

  def runSleekValidation(): Unit = {
    val testableFiles = listTestableDirs() flatMap listValidateableInDir

    println("Testable files:")
    testableFiles.foreach(println)

    val foldersUsed = testableFiles.map(_.getParent)

    val repoC = repo.identify()

    // run these using ValidateableSleekTestcase
    // making use of alt-run-tests, whatever.

    runTestsWith(repoC, foldersUsed) { case (binDir, corpusDir, repoRevision) =>
      // XXX readjust so as to not use archives every fucking time
      def runTest(tc: Testable with ExpectsOutput): TestCaseResult = {
        // Ideally, preparedSys would itself do the building of repo.
        // i.e. building the repo would be delayed until necessary.
        // At the moment, though, since any system loading tests will *have* the
        // tests, this is not going to slow things down.
        lazy val preparedSys = PreparedSystem(binDir, corpusDir)

        val resultsFor = runTestCaseForRevision(repoC, preparedSys)(ValidateableSleekTestCase.constructTestCase)

        // by this point,
        // tc *must* have proper expectedOutput
        resultsFor(tc)
      }

      allTestable foreach(tc => {
        val tcr = runTest(tc)
        tcr.displayResult()
      })
    }
  }
}
