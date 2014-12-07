package systemTestingDSL

package object consoleUtilities {

  def error(errorText: String) = println(Console.RED + errorText + Console.RESET)

  def success(successText: String) = println(Console.GREEN + successText + Console.RESET)

  def log(logText: String) = println(Console.YELLOW + logText + Console.RESET)
}