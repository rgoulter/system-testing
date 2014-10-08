package systemTestingDSL

object Usage {
	def main(args: Array[String]): Unit = {
			val x = new Test(commandName = "ls",
					arguments = "-lart").execute
					println(x)
	}
}