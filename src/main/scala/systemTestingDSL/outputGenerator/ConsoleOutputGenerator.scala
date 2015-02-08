package systemTestingDSL.outputGenerator

import systemTestingDSL.consoleUtilities

trait ConsoleOutputGenerator extends OutputGenerator {

  def had(hadResult: String) = consoleUtilities.actual(s"Had: $hadResult")

  def expected(expectedResult: String) = consoleUtilities.expected(s"Expected: $expectedResult")

}