package reportingDSL

import java.io.FileNotFoundException

class Reporter(
  repository: String,
  timePeriod: Int,
  outputDirectoryLocation: String,
  outputDirectoryName: String,

  testName: String,
  testCommand: String,
  testDirectory: String,
  testOutputPrefix: String = "",
  testOutputFileExtension: String = ".out") extends Execute {

  type Branch = String

  def setup(): Unit = {
    Utilities.ensureOutputDirectoryExists(outputDirectoryLocation, outputDirectoryName)
    HgApi.pull(repository)
  }

  def reset(original: Branch): Unit = {
    HgApi.checkoutBranch(repository, original)
  }

  def getOutputDirectoryName(branch: Branch): String = {
    outputDirectoryLocation.concat(outputDirectoryName).concat("/").concat(branch).concat("/").concat(testName)
  }

  def getOutputFileName(directoryName: String, commit: Commit): String = {
    directoryName.concat(commit.revisionId).concat("_").concat(Utilities.convertTimestampToString(commit.date)).concat(testOutputFileExtension)
  }

  def runTestsOnCommit(branch: Branch, commit: Commit) = {
    HgApi.revert(repository)
    this.runTest(commit, branch)
  }

  def runTest(commit: Commit, branch: Branch) = {
    val directoryName = getOutputDirectoryName(branch)
    val fullyQualifiedOutputFileName = getOutputFileName(directoryName, commit)
    if (Utilities.fileOrDirectoryExists(fullyQualifiedOutputFileName)) {
      println("Running test on commit ", commit.hash)
      Utilities.writeToFile(fullyQualifiedOutputFileName, executeInDirectory(testCommand, testDirectory).toString)
      println(s"""Output stored in directory $fullyQualifiedOutputFileName""")
    }
  }

  def processBranch(branch: Branch) = {
    try {
      HgApi.checkoutBranch(repository, branch)
      if (Utilities.checkLastCommitDate(HgApi.getLastCommitDate(repository, branch), timePeriod)) {
        println(s"""Running tests on $branch""")
        Utilities.createDirectory(outputDirectoryLocation.concat(outputDirectoryName).concat("/").concat(branch).concat("/").concat(testName))
        val commits = HgApi.getCommitList(repository)
        commits.foreach(commit => {
          println(commit.date + "|" + commit.hash )
          if (Utilities.checkLastCommitDate(commit.date, timePeriod))
            runTest(commit, branch)
        })

      }
    } catch {
      case _: Exception => println("Error occured")
    }
  }

  def runTests(): Unit = {
    setup
    val currentBranch = HgApi.getCurrentBranch(repository)
    println(s"""The current branch is $currentBranch""")
    val branches: Seq[String] = HgApi.listAllBranches(repository)
    branches.foreach(branch => processBranch(branch.split(" ")(0)))
    reset(currentBranch)
  }
}