High-Performance-DSLs
=====================

Final Year Project at the National University of Singapore. Final product wil be a DSL for system level testing and report parsing.

Installation
============

1. Please install the following dependencies on your system:

    * scala
    * sbt
    * pip
    * pyyaml module: `pip install pyyaml`
    * git

2. Clone the repository: 

`git clone https://github.com/rohitmukherjee/High-Performance-DSLs.git`

3. Currently the system already has the existing `run-fast-tests.pl` *hip* and *sleek* tests added to them. However, you will need to point the system to your local hg checkout to the `hg/sleek/hip/working/example/hip/` and
`hg/sleek/hip/working/example/sleek/` directories respectively. 

4. The above step can be performed by opening up 
`src/main/resources/application.conf` in a text - editor of you choice and configuring the directories.

5. Now, just to test whether the system is working - go to the root directory of the repository and run the following
    * sbt "run hip" - *to run hip tests*
    * sbt "run sleek" - *to run sleek tests*
    * sbt "run all" - *to first run sleek and then hip tests*
    * sbt "test" - *to run unit tests*

6. You should now see the project build and then start running tests. You should see output similar to the screenshot below on your screen.

<!-- ![Tests running](/Users/rohitmukherjee/dev/repositories/scalaWorkspace/High-Performance-DSLs/docs/screenshots/sleek.png "Tests Running") -->

![Tests running](/docs/screenshots/sleek.png?raw=true "Tests Running")

7. This setup describes how to run the set of tests on the existing branch and revision in the repository specified.

Modifying Existing Tests
========================

Modifying/Adding Hip Tests
----------------------------
1. All the test invocations for hip are in the file:
`src/main/scala/systemTestingDSL/HipTestSuiteUsage.scala`

2. Just modify the parameters in the test to tweak it. You will notice that the tests are split up into different methods which addTest to the suite, to add more tests you can do this or you can directly write in an addTest in the `run()` method.
    *  The format for the addTest(params...) is addTest("hip", source_file_location, arguments, output_directory_name,output_file_name,test_results)

    *  test_results are of the format: `method_1: SUCCESS/FAIL, method_2: SUCCESS/FAIL, ..., method_n: SUCCESS/FAIL`

    *  Eg:` suite addTest ("hip", BASE_DIR + "infinity/inflist.ss", "--dsd --en-inf", "results", "inflist.out", "remove: SUCCESS, append: SUCCESS")`

Modifying/Adding Sleek Tests
----------------------------
1. All the test invocations for sleek are in the file:
`src/main/scala/systemTestingDSL/SleekTestSuiteUsage.scala`

2. Just modify the parameters in the test to tweak it. You will notice that the tests are split up into different methods which addTest to the suite, to add more tests you can do this or you can directly write in an addTest in the `run()` method.
    *  The format for the addTest(params...) is addTest("sleek", source_file_location, arguments, output_directory_name,output_file_name, test_results)

    *  test_results are of the format: `Fail/Valid, Fail/Valid, Fail/Valid .......`

    *  Eg:  'suite addTest ("sleek", WORKING_DIR + "/sleek/label-basic.slk", " --dis-eps", "results", "label_basic", "Fail, Valid, Valid, Fail")'

Reports and Presentations
-------------------------

1. The midterm report is in the reports folder. Generated pdf from the tex source is provided. 
2. The presentation for the CA evaluation is in the presentations directory.Generated pdf from the text source is provided.

References
-------------------------

* DSL's in action: Debasish Ghosh [http://www.manning.com/ghosh/]
