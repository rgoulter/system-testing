package systemTestingDSL.outputGenerator

trait HTMLOutputGenerator extends OutputGenerator {

  def had(hadResult: String): Unit = println(s"Had: $hadResult")

  def expected(expectedResult: String): Unit = println(s"Expected: $expectedResult")

}