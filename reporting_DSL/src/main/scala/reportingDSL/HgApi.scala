package reportingDSL

object HgApi extends Execute {
  type Branch = String

  def listAllBranches(repository: String): Seq[String] = {
    executeInDirectory(HgCommands.listAllBranches, repository)
  }

  def pull(repository: String): Seq[String] = {
    executeInDirectory(HgCommands.pull, repository)
  }

  def checkoutBranch(repository: String, branch: Branch): Seq[String] = {
    executeInDirectory(HgCommands.update + " " + branch, repository)
  }

  def checkoutCommit(repository: String, commit: String): Seq[String] = {
    executeInDirectory(HgCommands.update + " " + commit, repository)
  }

  def getLastCommitDate(repository: String, branch: Branch): String = {
    executeInDirectory(HgCommands.lastCommitDate + " " + branch, repository)(0).split(" ")(0).substring(1)
  }

  def revert(repository: String): Unit = {
    executeInDirectory(HgCommands.revert, repository)
  }

  def getCommitList(repository: String): Seq[Commit] = {
    executeInDirectory(HgCommands.commitList, repository).dropRight(1).map(commit => {
      val commitInfo = commit.replace(""""""", "").split("_")
      Commit(revisionId = commitInfo(0).trim(), hash = commitInfo(1), date = commitInfo(2).split(" ")(0))
    })
  }
}