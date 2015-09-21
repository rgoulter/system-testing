package edu.nus.systemtesting.hipsleek

import org.stringtemplate.v4.STGroupString
import scala.io.Source

/**
 * Used for generating Scala files, in tandem with [[RunFastTests]] class.
 * Uses `StringTemplate 4` to do this.
 * @author richardg
 */
object SuiteGenerator {
  def main(args: Array[String]): Unit = {
  val TemplateGroup = "TestSuiteUsage.stg"
  val tgIS = getClass.getClassLoader.getResourceAsStream(TemplateGroup)
  assert (tgIS != null)
  val tgContent = Source.fromInputStream(tgIS).mkString
  val testSuiteSTG = new STGroupString(tgContent)


  val template = testSuiteSTG.getInstanceOf("suite"); 

  template.add("name", "Hello")
  template.add("pkg", "world")
  template.add("files", Array("f1", "f2", "f3"))

  val output = template.render()

  println(output)
  }
}

class SuiteGenerator