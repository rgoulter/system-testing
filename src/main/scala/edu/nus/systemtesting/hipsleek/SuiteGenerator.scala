package edu.nus.systemtesting.hipsleek

import scala.beans.BeanProperty
import scala.io.Source

import org.stringtemplate.v4.STGroupString

/**
 * Used for generating Scala files, in tandem with [[RunFastTests]] class.
 * Uses `StringTemplate 4` to do this.
 * @author richardg
 */
object SuiteGenerator {
  /**
   * For assisting with writing the test generation.
   *
   * @param args may be null
   */
  private class Test(@BeanProperty val command: String,
                     @BeanProperty val args: String,
                     @BeanProperty val filename: String)

  private class TestSet(@BeanProperty val name: String,
                        @BeanProperty val tests: Array[Test])

  private val sampleTestSets = {
    // Sample
    val stTests = Array(new Test("cmd", "", "file1"),
                        new Test("cmd", "args", "file2"),
                        new Test("cmd", "", "file3"))
    val stTestSets = Array(new TestSet("example", stTests),
                           new TestSet("example", stTests))

    stTestSets
  }

  def renderSuiteTemplate(): String = {
    val TemplateGroup = "TestSuiteUsage.stg"
    val tgIS = getClass.getClassLoader.getResourceAsStream(TemplateGroup)
    assert (tgIS != null)

    val tgContent = Source.fromInputStream(tgIS).mkString
    val testSuiteSTG = new STGroupString(tgContent)

    val template = testSuiteSTG.getInstanceOf("suite"); 

    val stTestSets = sampleTestSets

    template.add("name", "Hello")
    template.add("pkg", "world")
    template.add("examplesDir", "examples/sample")
    template.add("command", "myCmd")
    template.add("testSets", stTestSets)

    template.render()
  }

  def foo(x: Int) = println(x)
  def foo(x: Int, y: Int) = println(x)

  def main(args: Array[String]): Unit = {
    val output = renderSuiteTemplate

    println(output)
  }
}

class SuiteGenerator