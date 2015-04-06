package reportingDSL

object HgCommands {

  val listAllBranches = "hg branches"

  val pull = "hg pull"

  val currentBranch = "hg branch"

  val update = "hg update -C"

  val checkoutCommit = "hg update -c"

  val lastCommitDate = """hg log -l 1 -T "{date|hgdate}\n" --only-branch"""

  val log = "hg log"

  val revert = "hg revert -C -r "

  val whitespace = ' '

  val maxNumber = "-l50"

  val templateOption = """-T"""

  val commitTemplate = """"{node}_{date|hgdate}\n""""

  val localCommitTemplate = """"{rev}_{date|hgdate}\n""""

  val commitList = log + whitespace + maxNumber + whitespace + templateOption + whitespace + """"{rev}_{node}_{date|hgdate}\n""""

}