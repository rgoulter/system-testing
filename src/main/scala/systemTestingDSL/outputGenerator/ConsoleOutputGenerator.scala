package systemTestingDSL.outputGenerator

trait ConsoleOutputGenerator extends OutputGenerator {

  def error(errorText: String): String = Console.RED + errorText + Console.RESET

  def actual(errorText: String): String = Console.BLUE + errorText + Console.RESET

  def expect(errorText: String): String = Console.MAGENTA + errorText + Console.RESET

  def success(successText: String): String = Console.GREEN + successText + Console.RESET

  def log(logText: String): String = Console.YELLOW + logText + Console.RESET
}