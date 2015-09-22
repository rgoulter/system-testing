package edu.nus.systemtesting.hipsleek

import scala.beans.BeanProperty
import scala.io.Source

import org.stringtemplate.v4.ST
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
  case class Test(@BeanProperty args: String,
                  @BeanProperty filename: String,
                  @BeanProperty expectStr: String)

  private class TestSet(@BeanProperty val name: String,
                        @BeanProperty val tests: Array[Test])

  private val sampleTestSets = {
    // Sample
    val stTests = Array(new Test("", "file1", "true"),
                        new Test("args", "file2", "true"),
                        new Test("", "file3", "true"))
    val stTestSets = Array(new TestSet("example", stTests),
                           new TestSet("example", stTests))

    stTestSets
  }

  private def hipTestFromRFT(rft: RunFastTests.HipTest): Test = {
    // type HipTest = (String, String, List[(String, String)])
    val (filename, args, expectList) = rft
    val expectStr = expectList map { case (k,v) => s"$k: $v" } mkString(", ")

    new Test(args.trim(), filename, expectStr)
  }

  private def sleekTestFromRFT(rft: RunFastTests.SleekTest): Test = {
    // type SleekTest = (String, String, List[String])
    val (filename, args, expectList) = rft
    val expectStr = expectList mkString(", ")

    Test(args.trim(), filename, expectStr)
  }

  private def testSetsFromRFT[T](rft: List[(String, List[T])], toTest: T => Test):
      (Array[TestSet], Array[String]) = {
    // Ensure here that the (cmd, arg, fn) i.e. Test is unique.
    // It makes no sense that it wouldn't be.

    // need to check if (cmd, args, filename) is unique
    def key(t: Test) = (t.filename, t.args)

    var duplicateWarnings = Seq[String]()

    val res = rft map { case (name, tests) =>
      // name, as given, is 'python style', e.g. sleek_vperm
      val scName = name split "_" map { _.capitalize } mkString

      // fold, so can check whether unique
      val uniq = tests.foldLeft(Seq[Test]())({ (seq, testT) =>
        val test = toTest(testT)
        if (seq.exists { t => key(t) == key(test) }) {
          // Duplicate (cmd, args, filename) *in the same set*.
          System.err.println(s"Warning! Duplicate test in $name: $test. Ignoring.")
          duplicateWarnings = duplicateWarnings :+ s"Duplicate in $name: (${test.filename}, ${test.args}) w/ ${test.expectStr}"
          seq
        } else {
          seq :+ test
        }
      })

      new TestSet(scName, uniq.toArray)
    }

    // Just to be sure, check for uniqueness of (cmd, args, filename) across
    // each TestSet.
    val allTests = res map { _.tests } flatten

    allTests.foldLeft(Set[(String, String)]()) { (set, test) =>
      val k = key(test)
      if (set contains k) {
        System.err.println(s"Warning! Duplicate test: $test. Ignoring.")
        duplicateWarnings = duplicateWarnings :+ s"Duplicate: (${test.filename}, ${test.args}) w/ ${test.expectStr}"
        set
      } else {
        set + k
      }
    }

    (res.toArray, duplicateWarnings.toArray)
  }

  def renderSuiteTemplate(addAttributes: ST => Unit): String = {
    val TemplateGroup = "TestSuiteUsage.stg"
    val tgIS = getClass.getClassLoader.getResourceAsStream(TemplateGroup)
    assert (tgIS != null)

    val tgContent = Source.fromInputStream(tgIS).mkString
    val testSuiteSTG = new STGroupString(tgContent)

    val template = testSuiteSTG.getInstanceOf("suite"); 

    addAttributes(template)

    template.render()
  }

  def renderSleekTemplate(rft: List[(String, List[RunFastTests.SleekTest])], rev: String): String =
    renderSuiteTemplate { template =>
      val (testSets, warnings) = testSetsFromRFT(rft, sleekTestFromRFT)

      template.add("name", "Sleek")
      template.add("pkg", "edu.nus.systemtesting.hipsleek")
      template.add("revision", rev)
      template.add("examplesDir", "examples/working/sleek")
      template.add("command", "sleek")
      template.add("testSets", testSets)
      template.add("warnings", warnings)
    }

  def renderHipTemplate(rft: List[(String, List[RunFastTests.HipTest])], rev: String): String =
    renderSuiteTemplate { template =>
      val (testSets, warnings) = testSetsFromRFT(rft, hipTestFromRFT)
      template.add("name", "Hip")
      template.add("pkg", "edu.nus.systemtesting.hipsleek")
      template.add("revision", rev)
      template.add("examplesDir", "examples/working/hip")
      template.add("command", "hip")
      template.add("testSets", testSets)
      template.add("warnings", warnings)
    }

  def main(args: Array[String]): Unit = {
    // Hardcoded here, but not something we need to set often, so.
    val runFastTestsFilename = "/home/richardg/hg/sleekex/examples/working/run-fast-tests.pl"
    val revision = "79da9697f0c2"

    val rftSrc = Source.fromFile(runFastTestsFilename)
    val rftLines = rftSrc.getLines().toList
    rftSrc.close()

    val sleekRFT = RunFastTests.deriveSleekTests(rftLines)
    val hipRFT = RunFastTests.deriveHipTests(rftLines)

    val sleekOutput = renderSleekTemplate(sleekRFT, revision)
    val hipOutput = renderHipTemplate(hipRFT, revision)

    println(sleekOutput)
//    println(hipOutput)
  }
}

class SuiteGenerator