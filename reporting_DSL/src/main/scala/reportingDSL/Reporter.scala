package reportingDSL

import java.io.FileNotFoundException


class Reporter(builder: RepositoryTest) extends Execute {
  var repository = builder.repository
  var outputDirectoryLocation = builder.outputDirectoryLocation
  var testOutputPrefix = builder.testOutputPrefix
  var testOutputFileExtension = builder.testOutputFileExtension
  var testName = builder.testName
  var timePeriod = builder.timePeriod
  var testDirectory = builder.testDirectory
  var testCommand = builder.testCommand

  type Branch = String

  def setup(): Unit = {
    Utilities.ensureOutputDirectoryExists(outputDirectoryLocation, testName)
    HgApi.pull(repository)
  }

  def reset(original: Branch): Unit = {
    HgApi.checkoutBranch(repository, original)
  }

  def getOutputDirectoryName(branch: Branch): String = {
    outputDirectoryLocation.concat("/").concat(branch).concat("/").concat(testName)
  }

  def getOutputFileName(directoryName: String, commit: Commit): String = {
    directoryName.concat(testName).concat(commit.revisionId).concat("_").concat(Utilities.convertTimestampToString(commit.date)).concat(testOutputFileExtension)
  }

  def runTestsOnCommit(branch: Branch, commit: Commit) = {
    HgApi.checkoutCommit(repository, commit.hash)
    this.runTest(commit, branch)
  }

  def runTest(commit: Commit, branch: Branch) = {
    val directoryName = getOutputDirectoryName(branch)
    val fullyQualifiedOutputFileName = getOutputFileName(directoryName, commit)
    if (!Utilities.fileOrDirectoryExists(fullyQualifiedOutputFileName)) {
      println("Running test on commit ", commit.hash)
      val output = executeInDirectory(testCommand, testDirectory)
      var outputString = ""
      output.foreach(outputString += _ + "\n")
      Utilities.writeToFile(fullyQualifiedOutputFileName, outputString)
      println(s"""Output stored in directory $fullyQualifiedOutputFileName""")
    }
  }

  def processBranch(branch: Branch) = {
    HgApi.checkoutBranch(repository, branch)
    if (Utilities.checkLastCommitDate(HgApi.getLastCommitDate(repository, branch), timePeriod)) {
      println(s"""Running tests on $branch""")
      Utilities.createDirectory(outputDirectoryLocation.concat(testName).concat("/").concat(branch).concat("/").concat(testName))
      val commits = HgApi.getCommitList(repository)
      commits.foreach(commit => {
        if (Utilities.checkLastCommitDate(commit.date, timePeriod))
          runTest(commit, branch)
      })
    }
  }

  def runTests(): Unit = {
    setup
    val currentBranch = HgApi.getCurrentBranch(repository)
    println(s"""The current branch is $currentBranch""")
    val branches: Seq[String] = HgApi.listAllBranches(repository)
    try {
      branches.foreach(branch => processBranch(branch.split(" ")(0)))
    } catch {
      case _: Exception => println(s"""An exception occured, rolling back to $currentBranch""")
    } finally {
      reset(currentBranch)
    }
  }
}
