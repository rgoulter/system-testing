package systemTestingDSL

import scala.sys.process._

trait Runnable {
	def commandName: String
	def currentWorkingDirectory: String
	def fileName: String
	def arguments: String
	val separator: String = " "
	def execute: String = {
		val cmd = commandName.concat(separator).concat(fileName).concat(arguments)
				val result: String = cmd.!!
				result
}
}