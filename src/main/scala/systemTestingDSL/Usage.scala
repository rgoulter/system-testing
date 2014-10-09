package systemTestingDSL

object Usage {
	def main(args: Array[String]): Unit = {
		print(new TestCase(commandName = "sleek", fileName  = "/home/rohit/hg/sleek_hip/examples/sleek8.slk", arguments = "--elp") run)
	}
}
