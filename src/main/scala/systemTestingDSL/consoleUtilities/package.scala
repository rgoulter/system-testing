package systemTestingDSL

package object consoleUtilities {

  def error(errorText: String) = println(Console.RED + errorText + Console.RESET)

  def success(successText: String) = println(Console.GREEN + successText + Console.RESET)

  def expected(expectedText: String) = println(Console.BLUE + expectedText + Console.RESET)

  def actual(actualText: String) = println(Console.MAGENTA + actualText + Console.RESET)

  def log(logText: String) = println(Console.YELLOW + logText + Console.RESET)
}