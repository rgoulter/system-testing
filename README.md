# System Testing for Hip/Sleek

This project aims to provide a DSL for system level testing.
It's tailored to the [Hip/Sleek verification
tool](http://loris-7.ddns.comp.nus.edu.sg/~project/hip/index.html) in
particular.

Forked from a Final Year Project at the National University of Singapore.

# Building

This project uses the [Scala Built Tool (SBT)](http://www.scala-sbt.org/) to
build the project.  Use `sbt one-jar` to construct an uber-jar which contains
all the dependencies.

`sbt test` will run all the [ScalaTest](http://www.scalatest.org/) specs, under
`src/test/scala`.

With [global
plugins](http://www.scala-sbt.org/0.12.2/docs/Getting-Started/Using-Plugins.html#global-plugins)
for SBT, IDE-specific project files can be generated, e.g.  by `sbt eclipse` to
generate project files for [Scala IDE](http://scala-ide.org/download/sdk.html).

# Running

The program can be run directly from [SBT](http://www.scala-sbt.org/) using
`sbt "run <args>"`.  (*Note*: The quotation marks around `"run <args>"` are
important).  
With a JAR, (e.g. a pre-built one from
[releases](https://github.com/rgoulter/system-testing/releases)), the program
can be run using `java -jar /path/to/jar <args>`.

## Running Tests on a Single Commit

This project can be used to run some set of tests on some commit of a
repository.

e.g. where <args> is: `sleek [<rev>]`, `hip [<rev>]`, or `all [<rev>]` to
run the sleek tests, the hip tests, or both the sleek and hip tests
(respectively).

Results of the execution are stored under the `results/` folder. If the results
required are already stored, the tests are not run again, and the results from
these are displayed.

### Specifying a Revision

Passing some `<rev>` revision of the repository to test against
is optional. If given, the system tests runs on that commit, otherwise
the program uses the version of the working directory of the repository.

## Showing Difference in Test Results Across Commits

This project allows showing the differences between test results. This helps
show whether anything was broken relative to some previous commit, etc.  
This can be run using `diff --hip [<rev1> [<rev2>]]`, `diff --sleek [<rev1>
[<rev2>]]`, `diff --all [<rev1> [<rev2>]]`.  
Revisions (as above) are optional, and specified in older-to-newer order.

Specifying both revisions makes this explicit; specifying one revision shows
the diff of the given revision against the working directory of the repo.
Running the diff command without specifying a revision will run the tests
against the parent(s) of the working directory.

If the results have not been computed for any of `(command, revision)` pairs
required, these are run in order to compute the diff.

## Configuration

If the JAR is run in a mercurial repository (or some descendant of a mercurial
repository), the program assumes this is the hip/sleek repo.

Aside from this, it will look for a `.hipsleektest.conf` file in the current
directory (or ancestors), for a config with a `REPO_DIR=/path/to/repo` setting.
If it fails to find this, it will attempt to load the `application.conf` from
the JAR resources (i.e. the `src/main/resources/application.conf` it was
compiled with).  
These `.conf` files are read by typesafehub/config, so can be written using
[HOCON](https://github.com/typesafehub/config#using-hocon-the-json-superset)
format. See `hipsleekconf.template` for an example of what this config file
should look like.

# Modifying Existing Tests

## Modifying/Adding Sleek/Hip Tests

See `SleekTestSuiteUsage` and `HipTestSuiteUsage` in package
`edu.nus.systemtesting.hipsleek`.

## Translating code from run-fast-tests.pl

If there's a need for this, see `RunFastTests` in package
`edu.nus.systemtesting.hipsleek`. The `SuiteGenerator` object makes use of
a template to generate the `TestSuiteUsage`-like classes from the structure
`RunFastTests` parses.

# Repository Maintenance

From rohitmuhkerjee/High-Performance-DSLs, the `docs/` folder had been
committed with several large (~MBs) files. These have been filtered from this
repository.
