package systemTestingDSL

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.sys.process._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.actors.threadpool.TimeoutException

trait Runnable {
  def commandName: String
  def fileName: String
  def arguments: String
  def outputFileName: String
  val separator: String = SPACE

  def execute: String = {
    val executeFuture: Future[String] = Future {
      val cmd = commandName.concat(separator).concat(arguments).concat(separator).concat(fileName)
      println(cmd)
      val result: String = cmd.!!
      result
    }
    try {
      Await.result(executeFuture, 300 seconds)
    } catch {
      case ex: TimeoutException => "The above computation timed out"
    }
  }

}
