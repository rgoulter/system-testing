package reportingDSL

class RepositoryTest {
  var repository: String = ""
  var timePeriod: Int = 0
  var outputDirectoryLocation: String = ""
  var testName: String = ""
  var testCommand: String = ""
  var testDirectory: String = ""
  var testOutputPrefix: String = ""
  var testOutputFileExtension: String = ".out"

  def within(_timePeriod: Int): RepositoryTest = {
    this.timePeriod = _timePeriod
    this
  }

  def on(_repository: String): RepositoryTest = {
    this.repository = _repository
    this
  }

  def storeOutputIn(_outputDirectoryLocation: String): RepositoryTest = {
    this.outputDirectoryLocation = _outputDirectoryLocation
    this
  }

  def called(_testName: String): RepositoryTest = {
    this.testName = _testName
    this
  }

  def run(_testCommand: String): RepositoryTest = {
    this.testCommand = _testCommand
    this
  }

  def inDirectory(_testDirectory: String): RepositoryTest = {
    this.testDirectory = _testDirectory
    this
  }

  def withPrefix(_prefix: String): RepositoryTest = {
    this.testOutputPrefix = _prefix
    this
  }

  def withExtension(_extension: String): RepositoryTest = {
    this.testOutputPrefix = _extension
    this
  }

  def build(): Reporter = {
    new Reporter(this)
  }
}
