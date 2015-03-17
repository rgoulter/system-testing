package systemTestingDSL

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.sys.process._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.actors.threadpool.TimeoutException
import com.typesafe.config.ConfigFactory
import java.io.FileNotFoundException

trait Runnable {
  def commandName: String
  def fileName: String
  def arguments: String
  def outputFileName: String
  val separator: String = SPACE

  def execute: String = {
    val cmd = commandName.concat(separator).concat(arguments).concat(separator).concat(fileName)
    val timeout: Int = ConfigFactory.load().getInt("TIMEOUT")
    val executeFuture: Future[String] = Future {
      println(cmd)
      val result: String = cmd.!!
      result
    }
    try {
      Await.result(executeFuture, 300 seconds)
    } catch {
      case ex: TimeoutException => "The above computation timed out"
      case ex: FileNotFoundException => "The file could not be found, please check the executable paths"
    }
  }

}
