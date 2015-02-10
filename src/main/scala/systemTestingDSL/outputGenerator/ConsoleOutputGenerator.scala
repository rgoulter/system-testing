package systemTestingDSL.outputGenerator

trait ConsoleOutputGenerator extends OutputGenerator {

  def had(hadResult: String) = actual(s"Had: $hadResult")

  def expected(expectedResult: String) = expect(s"Expected: $expectedResult")

  def writeToFile(name: String, directory: String, content: String) = super.writeToFile(name, directory, content, ".out")

  def passed = success("Passed")

  def failed = error("Failed")

  def error(errorText: String) = println(Console.RED + errorText + Console.RESET)

  def actual(errorText: String) = println(Console.BLUE + errorText + Console.RESET)

  def expect(errorText: String) = println(Console.MAGENTA + errorText + Console.RESET)

  def success(successText: String) = println(Console.GREEN + successText + Console.RESET)

  def log(logText: String) = println(Console.YELLOW + logText + Console.RESET)
}