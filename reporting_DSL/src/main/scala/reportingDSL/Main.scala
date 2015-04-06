package reportingDSL

object Usage {
  def main(args: Array[String]): Unit = {
    val firstTest =
      new RepositoryTest() called "sleek_tests" on "/home/rohit/hg/sleek_hip/" within 300 storeOutputIn "/home/rohit/High-Performance-DSLs/Reporting Tool/" run "./sleek.sh" inDirectory "/home/rohit/High-Performance-DSLs/" build

    firstTest runTests

  }
}
