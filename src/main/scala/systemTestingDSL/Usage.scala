package systemTestingDSL

object Usage {
	def main(args: Array[String]): Unit = {
		print(new TestCase(commandName = "ls", arguments = "-lart") run)
	}
}