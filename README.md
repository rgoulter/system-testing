System Testing for Hip/Sleek
============================

Forked from a Final Year Project at the National University of Singapore.
This project aims to provide a DSL for system level testing and report parsing.

Installation
============

1. Please install the following dependencies on your system:

    * scala
    * sbt
    * pip
    * pyyaml module: `pip install pyyaml`
    * git
    * hg version > 3.0.1

(The Python dependencies are required only for some helper scripts,
not for the main/core project).

2. Clone the repository:

`git clone https://github.com/rohitmukherjee/High-Performance-DSLs.git`

3. Currently the system already has the existing `run-fast-tests.pl` *hip* and
   *sleek* tests added to them. However, you will need to point the system to
   your local hg checkout to the `hg/sleek/hip/working/example/hip/` and
   `hg/sleek/hip/working/example/sleek/` directories respectively.

4. The above step can be performed by opening up
   `src/main/resources/application.conf` in a text - editor of you choice and
   configuring the directories.

5. Now, just to test whether the system is working - go to the root directory
   of the repository and run the following
    * `sbt "run hip"` - *to run hip tests*
    * `sbt "run sleek"` - *to run sleek tests*
    * `sbt "run all"` - *to first run sleek and then hip tests*
    * `sbt "test"` - *to run unit tests*

(*Note*: The quotation marks around `"run all"` are important).

6. This setup describes how to run the set of tests on the existing branch and
   revision in the repository specified.

Modifying Existing Tests
========================

Modifying/Adding Hip Tests
----------------------------

1. All the test invocations for hip are in the file:
`src/main/scala/systemTestingDSL/HipTestSuiteUsage.scala`

2. Just modify the parameters in the test to tweak it. You will notice that the
   tests are split up into different methods which addTest to the suite, to add
   more tests you can do this or you can directly write in an addTest in the
   `run()` method.
    *  The format for the addTest(params...) is addTest("hip",
       source_file_location, arguments,
       output_directory_name,output_file_name,test_results)

    *  test_results are of the format: `method_1: SUCCESS/FAIL, method_2:
       SUCCESS/FAIL, ..., method_n: SUCCESS/FAIL`

    *  e.g.: `suite addTest ("hip", BASE_DIR + "infinity/inflist.ss", "--dsd
       --en-inf", "results", "inflist.out", "remove: SUCCESS, append:
       SUCCESS")`

Modifying/Adding Sleek Tests
----------------------------

1. All the test invocations for sleek are in the file:
   `src/main/scala/systemTestingDSL/SleekTestSuiteUsage.scala`

2. Just modify the parameters in the test to tweak it. You will notice that the
   tests are split up into different methods which addTest to the suite, to add
   more tests you can do this or you can directly write in an addTest in the
   `run()` method.
    *  The format for the addTest(params...) is addTest("sleek",
       source_file_location, arguments, output_directory_name,output_file_name,
       test_results)

    *  test_results are of the format: `Fail/Valid, Fail/Valid, Fail/Valid
       .......`

    *  e.g.: `suite addTest ("sleek", WORKING_DIR + "/sleek/label-basic.slk", "
       --dis-eps", "results", "label_basic", "Fail, Valid, Valid, Fail")`

Translating code from run-fast-tests.pl
---------------------------------------

The tests in run-fast-tests.pl have been already translated and coded into the
existing system. However, if further translation is required, please follow the
following instructions.

Due to the complexity of the `run-fast-tests.pl`, there are 2 parser scripts
that do parsing from components of run-fast-tests.pl > DSL.

These scripts can be found in `/old_system/parsers/`. There are 2 parsers -
sleek_parser.py and hip_parser.py. The inputs taken by them are there in the
script files themselves as comments.

Examples of these inputs can be found in /old_system/source_translations/. Make
sure to change the **INPUT_FILE** field in the scripts based on the name of
your input.

Then just run, `python hip_parser.py` or python `sleek_parser.py`. It should
print the generated DSL to the console. Just copy this over to the
`src/main/scala/systemTestingDSL/SleekTestSuiteUsage.scala` or
`src/main/scala/systemTestingDSL/HipTestSuiteUsage.scala` based on whether you
are parsing hip/sleek.

References
-------------------------

* DSL's in action: Debasish Ghosh [http://www.manning.com/ghosh/]

Repository Maintenance
======================

From rohitmuhkerjee/High-Performance-DSLs, the `docs/` folder had been committed with several large (~MBs) files. These have been filtered from this repository.
