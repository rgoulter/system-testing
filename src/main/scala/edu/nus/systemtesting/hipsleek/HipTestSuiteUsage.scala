package edu.nus.systemtesting.hipsleek

import java.io.PrintWriter
import edu.nus.systemtesting.testsuite.TestSuite
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseBuilder

class HipTestSuiteUsage(hipCommand: String,
                        examplesDir: String,
                        significantTime: Long,
                        timeout: Int,
                        revision : String) extends ConstructHipTests {
  def test(cmd: String,
           file: String,
           args: String,
           expectedOutput: String): HipTestCase =
    (new TestCaseBuilder
       runCommand cmd
       onFile file
       withArguments args
       checkAgainst expectedOutput
       timeoutAfter timeout)

  def run(): Unit = {
    val tests =
    makeInfinityTests() ++
    makeArrayTests() ++
    makeListTests() ++
    makeTermTests() ++
    makeImmTests() ++
    //    makeThreadTests() ++
    makeHipBagaTests() ++
    makeMemTests() ++
    makeVeribsyncTests() ++
    makeParahipTests() ++
    //    makeVpermTests() ++
    makeHipTests() // ++
    //    makeHipBarrTests(),

//    test (hipCommand, examplesDir + "imm-field/sll.ss", "-tp oc --field-ann --etcsu1 ", "delete: SUCCESS, get_tail: SUCCESS, insert: SUCCESS, insert2: SUCCESS"),

    val suite = new TestSuite(tests, revision, significantTime)
    val suiteResult = suite.runAllTests
    suiteResult generateTestStatistics(new PrintWriter(System.out))
  }

  def makeInfinityTests(): List[TestCase] = {
    List(// Infinity
    test (hipCommand, examplesDir + "infinity/inflist.ss", "--dsd --en-inf", "remove: SUCCESS, append: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/infll_take.ss", "--dsd --en-inf", "take: SUCCESS"),

    //    test (hipCommand, examplesDir + "infinity/inftree.ss", "--dsd --en-inf", "count: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/stream.ss", "--dsd --en-inf", "zip: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/bst-inf.ss", "--dsd --en-inf", "delete: SUCCESS, remove_min: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/inf-selsort.ss", "--dsd --en-disj-compute --en-inf", "find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/inf-ins.ss", "--dsd --en-inf", "insert: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/inf-sel.ss", "--dsd --en-inf", "find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/bubble-inf.ss", "--dsd --en-inf", "id2: SUCCESS, id3: SUCCESS, bubble: SUCCESS, bsort: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/heaps-inf.ss", "--en-inf", "insert: SUCCESS, deleteoneel: SUCCESS, deleteone: SUCCESS, deletemax: SUCCESS"),

    test (hipCommand, examplesDir + "infinity/merge-inf.ss", "--dsd --en-inf --en-disj-compute", "merge: SUCCESS"))
  }

  def makeArrayTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "array/arr_at.java", "", "main: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_binarysearch.java", "", "binary_search: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_search_decrease_less_than_two.java", "", "searchzero: FAIL"),

    test (hipCommand, examplesDir + "array/arr_bubblesort.java", "", "bubblesort: SUCCESS, bubble: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_bubblesort_perm.java", "", "bubblesort: SUCCESS, bubble: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_double.java", "", "doublearr: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_extract_nonzeros.java", "", "copy_nonzeros: SUCCESS, count_nonzeros: SUCCESS, extract_nonzeros: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_init.java", "", "zinit: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_insertsort.java", "", "insertelm: SUCCESS, insertion_sort: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_insertsort_perm.java", "", "insertelm: SUCCESS, insertion_sort: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_invert.java", "", "Invert: SUCCESS, InvertHelper: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_max.java", "", "max_value_of_array: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_mergesort.java", "", "merge_sorted_arrays: SUCCESS, copy_array: SUCCESS, merge_sort: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_new_exp.java", "", "main: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_nqueens.java", "", "nQueens: SUCCESS, nQueensHelper: SUCCESS, nQueensHelperHelper: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_qsort.java", "", "arraypart: SUCCESS, qsort: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_rev.java", "", "arrayrev: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_selectionsort.java", "", "array_index_of_max: SUCCESS, selection_sort: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_selectionsort_perm.java", "", "array_index_of_max: SUCCESS, selection_sort: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_sparse.java", "--imm", "create: SUCCESS, get: SUCCESS, setsa: SUCCESS"),

    test (hipCommand, examplesDir + "array/arr_sum.java", "", "sigmaright: SUCCESS, sigmaleft: SUCCESS"))
  }

  def makeListTests(): List[TestCase] = {
    List(// Lists
    test (hipCommand, examplesDir + "lists/demo.ss", " ", ":  reverse,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/demo2.ss", " ", " app_rev:  SUCCESS,  reverse:  SUCCESS,  append:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/err-coq.ss", " ", " ret_first2:  SUCCESS,  ret_first:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll.ss", " ", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll_bak.ss", " ", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll_bak2.ss", " ", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll_bak3.ss", " ", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll_nolists.ss", " ", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll_test1.ss", " ", " reverse:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll_test2.ss", " ", " delete:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll_test4.ss", " ", " test:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/ll_test5.ss", " ", " delete_val:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/lrev-bug.ss", " ", " lrev:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/lrev.ss", " ", " lrev:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/merge.ss", " ", " merge:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/merge1.ss", " ", " merge:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/merge2.ss", " ", " merge:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/merge3.ss", " ", " merge:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/mk_zero.ss", " ", " mk_zero:  SUCCESS"),

    test (hipCommand, examplesDir + "lists/perm.ss", " ", " append:  SUCCESS"))
  }

  def makeTermTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "term/e1.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex1.ss", " ", " length:  SUCCESS,  app2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex10.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex11.ss", " ", " bsearch:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex15.ss", " ", " loop:  SUCCESS,  f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex16.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex2.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex3.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex4.ss", " ", " inc_loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex5.ss", " ", " foo:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex6.ss", " ", " Ack:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex7.ss", " ", " loop_aux1:  SUCCESS,  loop_aux:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex8.ss", " ", " loop2:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/ex9.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/mutual.ss", " ", " g:  SUCCESS,  f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/cav08-1.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/cav08-2.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/cav08-3.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/cav08-4.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/cav08-5.ss", " ", " loop:  SUCCESS,  f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/cav08-6.ss", " ", " gcd:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/dijkstra76-1.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/dijkstra76-2.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/dijkstra76-3.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/pldi06-1.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/pldi06-2.ss", " ", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/pldi06-3.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/pldi06-4.ss", " ", " main:  SUCCESS,  loop:  SUCCESS,  loop_aux:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/pldi06-5.ss", " ", " Ack:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/popl07-1.ss", " ", " loop_1:  SUCCESS,  loop_2:  SUCCESS,  loop_3:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/popl07-2.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/sas05.ss", " ", " loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/sas10-1.ss", " ", " f:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/sas10-2.ss", " ", " foo:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/sas10-2a.ss", " ", " foo:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/sas10-3.ss", " ", " main:  SUCCESS,  foo:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/vcc-1.ss", " ", " f:  SUCCESS,  g:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/vmcai05-1a.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/lit/vmcai05-1b.ss", " -tp redlog", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/AlternatingIncr.ss", " ", " increase:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/AlternDiv-invalid-1.ss", " -tp redlog", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/AlternDiv.ss", " -tp redlog", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/AlternDivWidening.ss", " -tp redlog", " loop:  SUCCESS,  loop_aux:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/AlternDivWide.ss", " ", " loop:  SUCCESS,  loop_aux:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/AlternKonv.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Collatz.ss", " ", " collatz:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/ComplInterv2.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/ComplInterv3.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/ComplInterv.ss", " -tp z3", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/ComplxStruc-may.ss", " ", " complxStruc:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/ComplxStruc2.ss", " ", " loop:  SUCCESS,  complxStruc:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/ConvLower.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Cousot.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/DoubleNeg.ss", " -tp redlog", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Even.ss", " ", " even:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex01.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex02.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex03.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex04.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex05.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex06.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex07.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex08.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Ex09.ss", " ", " half:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Fibonacci.ss", " ", " fib:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Flip2.ss", " ", " flip:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Flip3.ss", " ", " flip:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Flip.ss", " ", " flip:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Gauss.ss", " ", " sum:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Gcd-may.ss", " ", " gcd:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Lcm.ss", " ", " lcm:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Marbie1.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Marbie2.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Middle.ss", " ", " middle:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/MirrorIntervSim.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/MirrorInterv.ss", " ", " mirrorInterv:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/ModuloLower.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/ModuloUp.ss", " -tp redlog", " up:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Narrowing.ss", " ", " narrowing:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/NarrowKonv.ss", " ", " narrowKonv:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/NegPos.ss", " -tp redlog", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Plait-may.ss", " ", " plait:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/Sunset.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/TrueDiv.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/TwoFloatInterv.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/UpAndDownIneq.ss", " ", " upAndDown:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/UpAndDown.ss", " ", " upAndDown:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileBreak.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileDecr.ss", " ", " decrease:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileIncrPart.ss", " ", " increase:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileIncr.ss", " ", " increase:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileNestedOffset.ss", " ", " increase:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileNested.ss", " ", " increase:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhilePart.ss", " ", " increase:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileSingle.ss", " ", " increase:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileSum.ss", " ", " increase:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/key/WhileTrue.ss", " ", " endless:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/DivMinus2.ss", " ", " main:  SUCCESS,  div:  SUCCESS,  minus:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/DivMinus.ss", " ", " main:  SUCCESS,  div:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/DivWithoutMinus.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/Duplicate.ss", " ", " main:  SUCCESS,  round:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/GCD2.ss", " -tp redlog", " main:  SUCCESS,  gcd:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/GCD3.ss", " ", " main:  SUCCESS,  gcd:  SUCCESS,  mod:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/GCD4.ss", " ", " main:  SUCCESS,  gcd:  SUCCESS,  mod:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/GCD5.ss", " -tp redlog", " main:  SUCCESS,  gcd:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/GCD.ss", " -tp redlog", " main:  SUCCESS,  gcd:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/LogAG.ss", " ", " main:  SUCCESS,  half:  SUCCESS,  log:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/LogBuiltIn.ss", " ", " main:  SUCCESS,  log:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/LogIterative.ss", " -tp redlog", " main:  SUCCESS,  log:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/LogMult.ss", " -tp redlog", " main:  SUCCESS,  log:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/Log.ss", " ", " main:  SUCCESS,  half:  SUCCESS,  log:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/McCarthyIterative-may.ss", " ", " mcCarthy:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/McCarthyRec.ss", " ", " mcCarthy:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/MinusBuiltIn.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/MinusMin.ss", " ", " main:  SUCCESS,  mn:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/MinusUserDefined.ss", " ", " main:  SUCCESS,  gt:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/Mod.ss", " ", " main:  SUCCESS,  mod:  SUCCESS,  minus:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/PlusSwap.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Aprove_09/Round3.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/AProVE_10/AG313.ss", " ", " main:  SUCCESS,  quot:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/AProVE_11_iterative/RetValRec.ss", " ", " main:  SUCCESS,  ret:  SUCCESS,  test:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/AProVE_11_iterative/RetVal.ss", " ", " main:  SUCCESS,  ret:  SUCCESS,  test:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/AProVE11NO/LoopingNonTerm.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/AProVE11NO/NonPeriodicNonTerm2.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/Avg.ss", " ", " avg:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/EqUserDefRec.ss", " ", " main:  SUCCESS,  eq:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/Fibonacci.ss", " ", " main:  SUCCESS,  fib:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/LeUserDefRec.ss", " ", " main:  SUCCESS,  le:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/LogRecursive.ss", " -tp redlog", " main:  SUCCESS,  log:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/Nest.ss", " ", " main:  SUCCESS,  nest:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec01.ss", " ", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec02.ss", " -tp redlog", " fact:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec03.ss", " ", " f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec04-modified.ss", " ", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec04.ss", " ", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/TimesPlusUserDef.ss", " ", " main:  SUCCESS,  times:  SUCCESS,  plus:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/BOG_RTA_11/TwoWay.ss", " -tp redlog", " twoWay:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Break.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Continue1.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Continue.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/costa09-example_1.ss", " ", " incr:  SUCCESS,  add:  SUCCESS,  incr2:  SUCCESS,  add2:  SUCCESS,  incr3:  SUCCESS,  add3:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/costa09-example_2.ss", " -tp redlog", " main:  SUCCESS,  divBy:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/costa09-example_3.ss", " ", " main:  SUCCESS,  m:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc1-exc.ss", " ", " main:  SUCCESS,  rec_f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc2-exc.ss", " ", " main:  SUCCESS,  rec_f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc3-exc.ss", " ", " main:  SUCCESS,  rec_f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc4-exc.ss", " ", " main:  SUCCESS,  rec_f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc5-exc.ss", " ", " main:  SUCCESS,  rec_f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc-exc.ss", " ", " main:  SUCCESS,  rec_f:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc1-no.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc2-no.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc3-no.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc4-no.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc5-no.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Exc-no.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Loop1.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Nested.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/Sequence.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09/TestJulia4.ss", " -tp redlog", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Ackermann.ss", " ", " main:  SUCCESS,  ack:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Double-1.ss", " -tp redlog", " test:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Double2-1.ss", " ", " main:  SUCCESS,  test:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Double2.ss", " ", " main:  SUCCESS,  test:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Double3-1.ss", " ", " main:  SUCCESS,  test:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Double3.ss", " ", " main:  SUCCESS,  test:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Double.ss", " -tp redlog", " main:  SUCCESS,  test:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Factorial.ss", " -tp redlog", " main:  SUCCESS,  fact:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/FactSumList.ss", " -tp redlog", " doSum:  SUCCESS,  fact:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/FactSum.ss", " -tp redlog", " doSum:  SUCCESS,  fact:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Costa_Julia_09-recursive/Hanoi.ss", " ", " main:  SUCCESS,  sol:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Iterative/NonPeriodic.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Iterative/Test11.ss", " -tp redlog", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Iterative/Test2.ss", " ", " main:  SUCCESS,  iter:  SUCCESS,  add:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/AckR.ss", " ", " main:  SUCCESS,  ack:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/FibSLR.ss", " -tp redlog", " main:  SUCCESS,  fib:  SUCCESS,  doSum:  SUCCESS,  create:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/HanR.ss", " ", " main:  SUCCESS,  sol:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/Power.ss", " -tp redlog", " power:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/EvenOdd.ss", " ", " even:  SUCCESS,  odd:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/Recursions.ss", " ", " main:  SUCCESS,  rec0:  SUCCESS,  rec1:  SUCCESS, rec2:  SUCCESS,  rec3:  SUCCESS,  rec4:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/Test10.ss", " ", " main:  SUCCESS,  rec:  SUCCESS,  test:  SUCCESS,  descend:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/Test12.ss", " -tp redlog", " main:  SUCCESS,  rec:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_10_Recursive/Test1.ss", " ", " main:  SUCCESS,  rec:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/ChooseLife.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/Choose.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/Continue.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/Loop.ss", " -tp redlog", " main:  SUCCESS,  test:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_00.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_01.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_02.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_03.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_04.ss", " ", " main:  SUCCESS,  for_1:  SUCCESS,  for_2:  SUCCESS,  for_3:  SUCCESS,  for_4:  SUCCESS,  for_5:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_05.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_06.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_10.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_11.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_12.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_20.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_21.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_22.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_23.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/NO_24.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/Parts.ss", " ", " parts:  SUCCESS,  main:  SUCCESS,  for_1:  SUCCESS,  loop_1:  SUCCESS,  for_2:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/Julia_11_iterative/Swingers.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaA10.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaA1.ss", " ", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaA4.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaA5.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaA6.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaA7.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaA8.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaA9.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB10.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB11.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB12.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB13.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB14.ss", " ", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB15.ss", " ", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB16-loop.ss", " ", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB16.ss", " ", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB17.ss", " ", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB18.ss", " ", " main:  SUCCESS,  loop:  SUCCESS,  decrease:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB1.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB2.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB3.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB4.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB4-loop.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB5.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB6.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB7.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaB8.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC10-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC11.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC11-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC1.ss", " ", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC1-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC2-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC3.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC3-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC4-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC5-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC7-simpl-1.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC7-simpl-2.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC7-simpl.ss", " ", " loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC7.ss", " ", " main:  SUCCESS,  loop:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC7-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC8-while.ss", " ", " main:  SUCCESS"),

    test (hipCommand, examplesDir + "term/benchs/aprove/pasta/PastaC9-while.ss", " ", " main:  SUCCESS"))
  }

  def makeImmTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "imm/bigint.ss", "   --imm -tp redlog", " clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS"),

    test (hipCommand, examplesDir + "imm/bigint_imm.ss", "   --imm -tp redlog", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, test:  SUCCESS,  add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult2:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS"),

    test (hipCommand, examplesDir + "imm/bigint_imm-star.ss", "   --imm -tp redlog", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS"),

    test (hipCommand, examplesDir + "imm/bigint-tight.ss", "   --imm -tp redlog", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS"),

    test (hipCommand, examplesDir + "imm/bigint-tight-imm.ss", "   --imm -tp redlog", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, test:  SUCCESS,  add_c:  SUCCESS, add:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS"),

    test (hipCommand, examplesDir + "imm/bigint-tight-imm-star.ss", "   --imm -tp redlog", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS"),

    test (hipCommand, examplesDir + "imm/append_imm.ss", "   --imm ", " append:  SUCCESS"),

    test (hipCommand, examplesDir + "imm/kara.ss", "   --imm -tp redlog", " karatsuba_mult: SUCCESS"),

    test (hipCommand, examplesDir + "imm/kara-imm-star.ss", "   --imm -tp redlog ", " karatsuba_mult: SUCCESS"),

    test (hipCommand, examplesDir + "imm/kara-imm-conj.ss", "  --imm -tp redlog", " karatsuba_mult: SUCCESS"),

    test (hipCommand, examplesDir + "imm/ll_imm.ss", "   --imm ", " length:  SUCCESS, append:  SUCCESS, sumN:  SUCCESS, set_next:  SUCCESS, get_next_next:  SUCCESS, get_next:  SUCCESS"),

    test (hipCommand, examplesDir + "imm-field/imspd.ss", "-tp oc --field-ann --etcsu1 ", "check_pass: SUCCESS, login: SUCCESS"),

    test (hipCommand, examplesDir + "imm-field/getset.ss", "-tp oc --field-ann --etcsu1 ", "sset: SUCCESS, get: SUCCESS, setA: SUCCESS, getA: SUCCESS, non_negative: SUCCESS"),

    test (hipCommand, examplesDir + "imm-field/bigint.ss", "-tp redlog --field-ann --etcsu1 ", "clone: SUCCESS, add_one_digit: SUCCESS, add_c: SUCCESS, add: SUCCESS, div_with_remainder: SUCCESS, bigint_of: SUCCESS, compare_int: SUCCESS, is_zero: SUCCESS, compare2: SUCCESS, int_value: SUCCESS, mult_c: SUCCESS, shift_left: SUCCESS, mult: SUCCESS, sub_one_digit: SUCCESS, sub_c: SUCCESS"),

    test (hipCommand, examplesDir + "imm-field/insertion_simple.ss", "-tp oc --field-ann --etcsu1 ", "insert: SUCCESS"),

    test (hipCommand, examplesDir + "imm-field/schorr-waite-list.ss", "-tp om --field-ann --etcsu1 ", "lscan: SUCCESS"))
  }

  def makeThreadTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "threads/motiv-example.ss", "  --en-para --en-thrd-resource -tp redlog", " main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "threads/motiv-example2.ss", "  --en-para --en-thrd-resource -tp redlog", " main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "threads/no-deadlock-nonlexical2.ss", "  --en-para --en-thrd-resource -tp parahip --en-lsmu-infer", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "threads/no-deadlock-nonlexical.ss", "  --en-para --en-thrd-resource -tp parahip --en-lsmu-infer", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "threads/forkjoin.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/cell4.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " inc: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/ls-bind.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "threads/deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "threads/deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "threads/disj-no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/disj-no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/disj-no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/disj-deadlock.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "threads/ordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/unordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " func: FAIL, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/oracle.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " thread: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/owicki-gries.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " incrementor2: SUCCESS, incrementor1: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "threads/fibonacci.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", " seq_fib: SUCCESS, para_fib: SUCCESS"),

    test (hipCommand, examplesDir + "trees.ss", "", "insert: SUCCESS"))
  }

  def addBagsTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "bags/avl-all-1.ss", " ", " remove_min:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-all.ss", " ", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS, remove_max_add:  SUCCESS,  : remove_min_add, insert:  SUCCESS,  rotate_double_left:   SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-modular-2.ss", " ", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-modular-3.ss", " ", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS, remove_max_add:  SUCCESS,  : remove_min_add, insert:  SUCCESS,  rotate_double_left:   SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-modular-2.ss", " ", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS, height: SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-modular-hei.ss", " ", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  rotate_right2:  SUCCESS,  rotate_left2:  SUCCESS,  height: SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-modular-new3.ss", " ", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  is_mem:  SUCCESS, rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS,  height: SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-modular-set.ss", "", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-modular-siz.ss", " ", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl-modular.ss", " ", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height: SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl.scp.ss", "", " delete:  SUCCESS,  remove_min:  SUCCESS,  insert_inline1:  SUCCESS,  insert_inline:  SUCCESS,  insert1:  SUCCESS,  insert:  SUCCESS, build_avl2:  SUCCESS,  build_avl1:  SUCCESS,  rotate_double_right1:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left1:  SUCCESS, rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right1:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left1:  SUCCESS,  rotate_left:  SUCCESS, height1:  SUCCESS,  height:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/avl.ss", " ", " insert_inline:  SUCCESS,   insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS, height:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/bubble.ss", " ", " bsort1:  SUCCESS,  bubble1:  SUCCESS,  id1:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/cll.ss", " ", " delete2:  SUCCESS,  delete:  SUCCESS,  count:  SUCCESS,  count_rest:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/dll.ss", " ", " append:  SUCCESS,  insert:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/insertion.ss", " ", " insertion_sort:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/ll.ss", " ", " reverse1:  SUCCESS,  delete1:  SUCCESS,  insert:  SUCCESS,  append:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/merge-modular.ss", " ", " insert1:  SUCCESS,  merge1:  SUCCESS,  merge_sort1:  SUCCESS,  split1:  SUCCESS,  count1:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/merge.ss", " ", " insert1:  SUCCESS,  merge1:  SUCCESS,  merge_sort1:  SUCCESS,  split1:  SUCCESS,  count1:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/qsort.ss", " ", " qsort1:  SUCCESS,  append_bll1:  SUCCESS,  partition1:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/rb_bags.ss", " ", " insert_1:  SUCCESS,  del_1:  SUCCESS,  remove_min_1:  SUCCESS,  del_2r_1:  SUCCESS,  del_2_1:  SUCCESS,  del_3r_1:  SUCCESS, del_3_1:  SUCCESS,  del_4r_1:  SUCCESS,  del_4_1:  SUCCESS,  del_5r_1:  SUCCESS,  del_5_1:  SUCCESS,  del_6r_1:  SUCCESS,  del_6_1:  SUCCESS, is_black_1:  SUCCESS,  is_red_1:  SUCCESS,  case_2r_1:  SUCCESS,  rotate_case_3r_1:  SUCCESS,  case_2_1:  SUCCESS,  rotate_case_3_1:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/rb.scp.ss", " ", " insert_1:  SUCCESS,  insert:  SUCCESS,  del_1:  SUCCESS,  del:  SUCCESS,  remove_min_1:  SUCCESS,  remove_min:  SUCCESS,  del_2r_1:  SUCCESS,  del_2r:  SUCCESS,  del_2_1:  SUCCESS,  del_2:  SUCCESS,  del_3r_1:  SUCCESS,  del_3r:  SUCCESS,  del_3_1:  SUCCESS,  del_3:  SUCCESS,  del_4r_1:  SUCCESS,  del_4r:  SUCCESS,  del_4_1:  SUCCESS,  del_4:  SUCCESS,  del_5r_1:  SUCCESS,  del_5r:  SUCCESS,  del_5_1:  SUCCESS,  del_5:  SUCCESS,  del_6r_1:  SUCCESS,  del_6r:  SUCCESS,  del_6_1:  SUCCESS,  del_6:  SUCCESS,  is_black_1:  SUCCESS,  is_black:  SUCCESS,  is_red_1:  SUCCESS,  is_red:  SUCCESS,  case_2r_1:  SUCCESS,  case_2r:  SUCCESS,  rotate_case_3r_1:  SUCCESS,  rotate_case_3r:  SUCCESS,  case_2_1:  SUCCESS,  case_2:  SUCCESS,  rotate_case_3_1:  SUCCESS,  rotate_case_3:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/selection.ss", " ", " selection_sort:  SUCCESS,  delete_min:  SUCCESS,  find_min:  SUCCESS"),

    test (hipCommand, examplesDir + "bags/trees.ss", " ", " delete1:  SUCCESS,  remove_min1:  SUCCESS,  insert1:  SUCCESS,  flatten1:  SUCCESS,  append1:  SUCCESS"))
  }

  def makeHipBagaTests(): List[TestCase] = {
    List(test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/eps.ss", " --baga-xpure", " get_next: SUCCESS, get_next_next: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/append.ss", " --baga-xpure", " append: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/append-tail.ss", " --baga-xpure", "append: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-bind.ss", " --baga-xpure", " height: SUCCESS,  rotate_left: SUCCESS,  rotate_right: SUCCESS,  get_max: SUCCESS,  rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl.ss", " --baga-xpure", " height: SUCCESS, rotate_left: SUCCESS, rotate_right: SUCCESS, get_max: SUCCESS, rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS, insert_inline: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-orig-2.ss", " --baga-xpure", "height: SUCCESS, get_max: SUCCESS, insert: SUCCESS, double_left_child: SUCCESS, double_right_child: SUCCESS, rotate_left_child: SUCCESS,  rotate_right_child: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-orig3.ss", " --baga-xpure", " height: SUCCESS, get_max: SUCCESS, insert: SUCCESS,  double_left_child: SUCCESS, double_right_child: SUCCESS,  rotate_left_child: SUCCESS, rotate_right_child: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/bll.ss", " --baga-xpure", " insert: SUCCESS, delete: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/bubble.ss", " --baga-xpure", " id2: SUCCESS, id3: SUCCESS, bubble: SUCCESS, bsort: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/cll.ss", " --baga-xpure", " test: SUCCESS, insert: SUCCESS, count_rest: SUCCESS, count: SUCCESS, delete: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/complete.ss", " --baga-xpure", " maxim: SUCCESS, minim: SUCCESS, height: SUCCESS, min_height: SUCCESS, insert: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/dll.ss", " --baga-xpure", " insert: SUCCESS, delete: SUCCESS, delete1: SUCCESS, test_del: SUCCESS, test_del2: SUCCESS, test_fold: SUCCESS, append: SUCCESS, append1: SUCCESS, f1: SUCCESS, f2: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/heaps.ss", " --baga-xpure", " insert: SUCCESS, deleteoneel: SUCCESS, deleteone: SUCCESS, ripple: SUCCESS, deletemax: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/insertion.ss", " --baga-xpure", " insert: SUCCESS, insertion_sort: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/ll.ss", " --baga-xpure", " append: SUCCESS, ret_first: SUCCESS, get_next: SUCCESS, set_next: SUCCESS, set_null: SUCCESS, get_next_next: SUCCESS, insert: SUCCESS, delete: SUCCESS, create_list: SUCCESS, reverse: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/merge.ss", " --baga-xpure", " count: SUCCESS, split_func: SUCCESS, merge_sort: SUCCESS, merge: SUCCESS, insert: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/perfect.ss", " --baga-xpure", " simple_insert: SUCCESS, create: SUCCESS, maxim: SUCCESS, height: SUCCESS, insert: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/qsort.ss", " --baga-xpure", " partition: SUCCESS, append_bll: SUCCESS, qsort: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/selection.ss", " --baga-xpure", " find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/sll.ss", " --baga-xpure", " insert: SUCCESS, insert2: SUCCESS, delete: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/trees.ss", " --baga-xpure", " append: SUCCESS, count: SUCCESS, flatten: SUCCESS, insert: SUCCESS, remove_min: SUCCESS, delete: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/rb.ss", " --baga-xpure", " rotate_case_3: SUCCESS, case_2: SUCCESS, rotate_case_3r: SUCCESS, case_2r: SUCCESS, is_red: SUCCESS, is_black: SUCCESS, del_6: SUCCESS, del_6r: SUCCESS, del_5: SUCCESS, del_5r: SUCCESS, del_4: SUCCESS, del_4r: SUCCESS, del_3: SUCCESS, del_3r: SUCCESS, del_2: SUCCESS, remove_min: SUCCESS, del: SUCCESS, insert: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/global-ll.ss", " --baga-xpure", " insert_rec: SUCCESS, delete_last_rec: SUCCESS, insert: SUCCESS, delete_last: SUCCESS, main: SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/dll-modular.ss", " --overeps --baga-xpure", "append:  SUCCESS,  append1:  SUCCESS,  append2:  SUCCESS, delete:  SUCCESS,  delete1:  SUCCESS,  f1:  SUCCESS, f2:  SUCCESS,  insert:  SUCCESS,  test_del:  SUCCESS, test_del2:  SUCCESS,  test_fold:  SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/selection-modular.ss", " --overeps --lda --baga-xpure", "delete_min:  SUCCESS,  find_min:  SUCCESS,  selection_sort:  SUCCESS"),

    test (hipCommand, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/qsort-modular.ss", " --overeps --lda --baga-xpure", "append_bll:  SUCCESS,  partition:  SUCCESS,  qsort:  SUCCESS"))
  }

  def makeMemTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "mem/dag.ss", "-tp om --mem --ramify", "mark: SUCCESS, mark2: SUCCESS"),

    test (hipCommand, examplesDir + "mem/dag_values.ss", "-tp om --mem --ramify", "mark: SUCCESS, mark2: SUCCESS"),

    test (hipCommand, examplesDir + "mem/dag_values_infer.ss", "-tp om --mem --ramify --infer-mem", "mark: SUCCESS, mark2: SUCCESS"),

    test (hipCommand, examplesDir + "mem/graph.ss", "-tp om --mem --ramify", "mark: SUCCESS, mark2: SUCCESS"),

    test (hipCommand, examplesDir + "mem/graph_values.ss", "-tp om --mem --ramify", "mark: SUCCESS, mark2: SUCCESS"),

    test (hipCommand, examplesDir + "mem/graph_values_infer.ss", "-tp om --mem --ramify --infer-mem", "mark: SUCCESS, mark2: SUCCESS"),

    test (hipCommand, examplesDir + "mem/dag_1.ss", "-tp om --mem --ramify", "mark: SUCCESS"),

    test (hipCommand, examplesDir + "mem/dag_1_ramify.ss", "-tp om --mem --ramify", "mark: SUCCESS"),

    test (hipCommand, examplesDir + "mem/graph_1.ss", "-tp om --mem --ramify", "mark: SUCCESS"),

    test (hipCommand, examplesDir + "mem/graph_1_ramify.ss", "-tp om --mem --ramify", "mark: SUCCESS"),

    test (hipCommand, examplesDir + "mem/dag_copy.ss", "-tp om --mem --ramify", "copy_dag: SUCCESS"),

    test (hipCommand, examplesDir + "mem/garbage_collector.ss", "-tp om --mem --ramify", "mark: SUCCESS, sweep: SUCCESS, collect: SUCCESS"),

    test (hipCommand, examplesDir + "mem/garbage_collector_values.ss", "-tp om --mem --ramify", "mark: SUCCESS, sweep: SUCCESS, collect: SUCCESS"),

    test (hipCommand, examplesDir + "mem/garbage_collector_values_infer.ss", "-tp om --mem --ramify --infer-mem", "mark: SUCCESS, sweep: SUCCESS, collect: SUCCESS"),

    test (hipCommand, examplesDir + "mem/llsortll.ss", "-tp om --mem --eps", "overlaid_insert: SUCCESS, delete2: SUCCESS, insert2: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS"),

    test (hipCommand, examplesDir + "mem/infer_llsortll.ss", "-tp om --mem --eps --infer-mem", "overlaid_insert: SUCCESS, delete2: SUCCESS, insert2: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS"),

    test (hipCommand, examplesDir + "mem/lltree.ss", "-tp om --mem", "move_request: SUCCESS"),

    test (hipCommand, examplesDir + "mem/infer_lltree.ss", "-tp om --mem --infer-mem", "move_request: SUCCESS"),

    test (hipCommand, examplesDir + "mem/lltree2.ss", "-tp om --mem --eps", "move_request: SUCCESS"),

    test (hipCommand, examplesDir + "mem/infer_lltree2.ss", "-tp om --mem --eps --infer-mem", "move_request: SUCCESS"),

    test (hipCommand, examplesDir + "mem/nodell.ss", "-tp om --mem --ramify", "delete_cache: SUCCESS, delete: SUCCESS, add_L: SUCCESS, caching: SUCCESS, add_in: SUCCESS, find: SUCCESS"),

    test (hipCommand, examplesDir + "mem/nodell_infer.ss", "-tp om --mem --ramify --infer-mem", "delete_cache: SUCCESS, delete: SUCCESS, add_L: SUCCESS, caching: SUCCESS, add_in: SUCCESS, find: SUCCESS"),

    test (hipCommand, examplesDir + "mem/pll.ss", "-tp om --mem", "length: SUCCESS, sum: SUCCESS"),

    test (hipCommand, examplesDir + "mem/pll_ramify.ss", "-tp om --mem --ramify", "length: SUCCESS, sum: SUCCESS"),

    test (hipCommand, examplesDir + "mem/pll_infer.ss", "-tp om --mem --infer-mem", "length: SUCCESS, sum: SUCCESS"),

    test (hipCommand, examplesDir + "mem/pll_ramify_infer.ss", "-tp om --mem --ramify --infer-mem", "length: SUCCESS, sum: SUCCESS"),

    test (hipCommand, examplesDir + "mem/doubly_circular_list.ss", "-tp om --mem", "insert_node_dcll: SUCCESS, insert_lln: SUCCESS, insert_llt: SUCCESS, insert_dcll: SUCCESS"),

    test (hipCommand, examplesDir + "mem/infer_doubly_circular_list.ss", "-tp om --mem --infer-mem", "insert_node_dcll: SUCCESS, insert_lln: SUCCESS, insert_llt: SUCCESS, insert_dcll: SUCCESS"),

    test (hipCommand, examplesDir + "mem/process_schedular.ss", "-tp om --mem", "insert_process: SUCCESS, insert_rll: SUCCESS, insert_pll: SUCCESS, insert_sll: SUCCESS"),

    test (hipCommand, examplesDir + "mem/process_schedular_ramify.ss", "-tp om --mem --ramify", "insert_process: SUCCESS, insert_rll: SUCCESS, insert_pll: SUCCESS, insert_sll: SUCCESS"),

    test (hipCommand, examplesDir + "mem/ramified-cells.ss", "--mem --ramify", "ex0: SUCCESS, ex1: SUCCESS, mark: SUCCESS"),

    test (hipCommand, examplesDir + "mem/ramified-pairs.ss", "--mem --ramify", "mark: SUCCESS"),

    test (hipCommand, examplesDir + "mem/jsvarstore.ss", "--mem --ramify", "ex1: SUCCESS"),

    test (hipCommand, examplesDir + "mem/graph_spanning.ss", "-tp om --mem --ramify --eps", "spanning: SUCCESS"),

    test (hipCommand, examplesDir + "mem/graph_spanning_infer.ss", "-tp om --mem --ramify --eps --infer-mem", "spanning: SUCCESS"))
  }

  def makeVeribsyncTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "veribsync/while-loop.ss", "  --en-para -perm bperm -tp redlog", "fun: SUCCESS, fun3: SUCCESS, loop_fun: SUCCESS, loop_fun3: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/while-loop2.ss", "  --en-para -perm bperm -tp redlog", "fun: SUCCESS, fun3: SUCCESS, fun4: SUCCESS, fun5: SUCCESS, fun6: SUCCESS, fun7: SUCCESS, fun8: SUCCESS, fun9: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/hip-bperm1.ss", "  --en-para -perm bperm -tp redlog", "destroyCellFail: FAIL, readCell: SUCCESS, testNewCell: SUCCESS, testNewCell2: SUCCESS, updateCell: SUCCESS, updateCellFail: FAIL"),

    test (hipCommand, examplesDir + "veribsync/bperm-exp.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-primitives.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-exp1.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-exp2.ss", "  --en-para -perm bperm -tp redlog", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-exp3.ss", "  --en-para -perm bperm -tp redlog", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-complex.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-complex2.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-complex3.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, thread: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-multiple.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, main_fail: SUCCESS, participant: SUCCESS, participant_fail: FAIL"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-consistency.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, main_fail: FAIL, participant: SUCCESS, participant1: SUCCESS, participant_fail: FAIL"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp1.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp2.ss", "  --en-para -perm bperm -tp redlog", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp3.ss", "  --en-para -perm bperm -tp redlog", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp4.ss", "  --en-para -perm bperm -tp redlog", "childthread1: SUCCESS, childthread2: SUCCESS, main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp5.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, thread: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp6.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp7.ss", "  --en-para -perm bperm -tp redlog", "CalculationInTask: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/benchmark/barnes.ss", "  --en-para -perm bperm -tp redlog", "ANLinit: SUCCESS,  ComputeForces: SUCCESS, Housekeep: SUCCESS, find_my_bodies: SUCCESS, hackcofm: SUCCESS, maketree: SUCCESS, stepsystem: SUCCESS, find_my_initial_bodies: SUCCESS, SlaveStart: SUCCESS, diagnostics: SUCCESS, initoutput: SUCCESS, initparam: SUCCESS, startrun: SUCCESS, tab_init: SUCCESS, main: SUCCESS, output: SUCCESS"))
  }

  def makeParahipTests() = {
    List(test (hipCommand, examplesDir + "parahip/simple.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/forkjoin.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/cell.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " test: SUCCESS, test1: FAIL"),

    test (hipCommand, examplesDir + "parahip/cell4.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " inc: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/cell-lock-vperm.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " testCell: SUCCESS, testVar: FAIL"),

    test (hipCommand, examplesDir + "parahip/cell-extreme-cases.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " test: FAIL, test2: FAIL, test3: FAIL, test4: FAIL"),

    test (hipCommand, examplesDir + "parahip/ls-bind.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/ls-waitlevel2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func2: SUCCESS, func3: SUCCESS, func4: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/double-acquire.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "parahip/no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "parahip/deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "parahip/deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "parahip/disj-no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/disj-no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/disj-no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/disj-deadlock.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: FAIL"),

    test (hipCommand, examplesDir + "parahip/ordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/unordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: FAIL, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/multicast.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " initialize: SUCCESS, thread: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/oracle.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " thread: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/owicki-gries.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " incrementor2: SUCCESS, incrementor1: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/fibonacci.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " seq_fib: SUCCESS, para_fib: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/create_and_acquire.ss", "  --en-para -tp parahip --dis-locklevel --en-thrd-and-conj", " create_and_acquire: SUCCESS"))
  }

  def makeVpermTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "vperm/vperm/alt_threading.ss", "  --ann-vp", " increment: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/fibonacci.ss", "  --ann-vp -tp z3 -perm none --dis-ls dis--locklevel", " seq_fib: SUCCESS, para_fib2: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/global-var-norace.ss", "  --ann-vp --dis-pgbv", " inc: SUCCESS, func: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/global-var-race.ss", "  --ann-vp --dis-pgbv", " inc: SUCCESS, func: FAIL"),

    test (hipCommand, examplesDir + "vperm/vperm/mergesort.ss", "  --ann-vp", " count: SUCCESS, split_func: SUCCESS, merge: SUCCESS, insert: SUCCESS, parallel_merge_sort2: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/passive_stack_race.ss", "  --ann-vp", " assign: SUCCESS, stack_race: FAIL"),

    test (hipCommand, examplesDir + "vperm/vperm/stack_race.ss", "  --ann-vp", " assign: SUCCESS, stack_race: FAIL"),

    test (hipCommand, examplesDir + "vperm/vperm/quicksort.ss", "  --ann-vp", " partition: SUCCESS, append_bll: SUCCESS, para_qsort2: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/task_decompose.ss", "  --ann-vp", " inc: SUCCESS, creator: SUCCESS, joiner: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/threads.ss", "  --ann-vp", " make_tree: SUCCESS, tree_compute_sum_facs: SUCCESS, summator: SUCCESS, start_sum_thread: SUCCESS, join_sum_thread: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/tree_count.ss", "  --ann-vp", " parallelCount2: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/tree_search.ss", "  --ann-vp -tp mona -perm none", " para_search2: SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm/vperm_check.ss", "  --ann-vp", " inc: SUCCESS, incCell: SUCCESS, test1: FAIL, test2: FAIL, test3: FAIL, test4: FAIL"),

    test (hipCommand, examplesDir + "vperm/vperm/vperm_simple.ss", "  --ann-vp", " foo: SUCCESS, f: SUCCESS, foo2: SUCCESS, f2: SUCCESS"))
  }

  def makeHipTests(): List[TestCase] = {
    List(test (hipCommand, examplesDir + "eps.ss", "  ", " get_next: SUCCESS, get_next_next: SUCCESS"),

    test (hipCommand, examplesDir + "append.ss", "  ", " append: SUCCESS"),

    test (hipCommand, examplesDir + "append-tail.ss", "  ", "append: SUCCESS"),

    test (hipCommand, examplesDir + "avl-bind.ss", "  ", " height: SUCCESS,  rotate_left: SUCCESS,  rotate_right: SUCCESS,  get_max: SUCCESS,  rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS"),

    test (hipCommand, examplesDir + "avl.ss", "	 ", "  height: SUCCESS, rotate_left: SUCCESS, rotate_right: SUCCESS, get_max: SUCCESS, rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS, insert_inline: SUCCESS"),

    test (hipCommand, examplesDir + "avl-orig-2.ss", "  ", "height: SUCCESS, get_max: SUCCESS, insert: SUCCESS, double_left_child: SUCCESS, double_right_child: SUCCESS, rotate_left_child: SUCCESS,  rotate_right_child: SUCCESS"),

    test (hipCommand, examplesDir + "avl-orig3.ss", " ", " height: SUCCESS, get_max: SUCCESS, insert: SUCCESS, 	double_left_child: SUCCESS, double_right_child: SUCCESS, 	rotate_left_child: SUCCESS, rotate_right_child: SUCCESS"),

    test (hipCommand, examplesDir + "bll.ss", "  ", " insert: SUCCESS, delete: SUCCESS"),

    test (hipCommand, examplesDir + "bubble.ss", "  ", " id2: SUCCESS, id3: SUCCESS, bubble: SUCCESS, bsort: SUCCESS"),

    test (hipCommand, examplesDir + "cll.ss", "  ", " test: SUCCESS, insert: SUCCESS, count_rest: SUCCESS, count: SUCCESS, delete: SUCCESS"),

    test (hipCommand, examplesDir + "complete.ss", " ", " maxim: SUCCESS, minim: SUCCESS, height: SUCCESS, min_height: SUCCESS, insert: SUCCESS"),

    test (hipCommand, examplesDir + "dll.ss", " ", " insert: SUCCESS, delete: SUCCESS, delete1: SUCCESS, test_del: SUCCESS, test_del2: SUCCESS, test_fold: SUCCESS, append: SUCCESS, append1: SUCCESS, f1: SUCCESS, f2: SUCCESS"),

    test (hipCommand, examplesDir + "heaps.ss", " ", " insert: SUCCESS, deleteoneel: SUCCESS, deleteone: SUCCESS, ripple: SUCCESS, deletemax: SUCCESS"),

    test (hipCommand, examplesDir + "insertion.ss", " ", " insert: SUCCESS, insertion_sort: SUCCESS"),

    test (hipCommand, examplesDir + "ll.ss", " ", " append: SUCCESS, ret_first: SUCCESS, get_next: SUCCESS, set_next: SUCCESS, set_null: SUCCESS, get_next_next: SUCCESS, insert: SUCCESS, delete: SUCCESS, create_list: SUCCESS, reverse: SUCCESS"),

    test (hipCommand, examplesDir + "merge.ss", " ", " count: SUCCESS, split_func: SUCCESS, merge_sort: SUCCESS, merge: SUCCESS, insert: SUCCESS"),

    test (hipCommand, examplesDir + "perfect.ss", " ", " simple_insert: SUCCESS, create: SUCCESS, maxim: SUCCESS, height: SUCCESS, insert: SUCCESS"),

    test (hipCommand, examplesDir + "qsort.ss", " ", " partition: SUCCESS, append_bll: SUCCESS, qsort: SUCCESS"),

    test (hipCommand, examplesDir + "selection.ss", " ", " find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS"),

    test (hipCommand, examplesDir + "sll.ss", " ", " insert: SUCCESS, insert2: SUCCESS, delete: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS"),

    test (hipCommand, examplesDir + "trees.ss", " ", " append: SUCCESS, count: SUCCESS, flatten: SUCCESS, insert: SUCCESS, remove_min: SUCCESS, delete: SUCCESS"),

    test (hipCommand, examplesDir + "rb.ss", " ", " rotate_case_3: SUCCESS, case_2: SUCCESS, rotate_case_3r: SUCCESS, case_2r: SUCCESS, is_red: SUCCESS, is_black: SUCCESS, del_6: SUCCESS, del_6r: SUCCESS, del_5: SUCCESS, del_5r: SUCCESS, del_4: SUCCESS, del_4r: SUCCESS, del_3: SUCCESS, del_3r: SUCCESS, del_2: SUCCESS, remove_min: SUCCESS, del: SUCCESS, insert: SUCCESS"),

    test (hipCommand, examplesDir + "global1.ss", " ", " increase: SUCCESS"),

    test (hipCommand, examplesDir + "global2.ss", " ", " increase: SUCCESS"),

    test (hipCommand, examplesDir + "global3.ss", " ", " increase: SUCCESS, increase_n: SUCCESS"),

    test (hipCommand, examplesDir + "global4.ss", " ", " increase_n: SUCCESS, main:  SUCCESS"),

    test (hipCommand, examplesDir + "global5.ss", " ", " increase: SUCCESS, decrease: SUCCESS"),

    test (hipCommand, examplesDir + "global-ll.ss", " ", " insert_rec: SUCCESS, delete_last_rec: SUCCESS, insert: SUCCESS, delete_last: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "global-mutual-rec.ss", " ", " decrease1: SUCCESS, decrease2: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "classic/classic1.ss", " ", " foo1:  SUCCESS,  foo2:  SUCCESS"),

    test (hipCommand, examplesDir + "classic/classic1.ss", " --classic", " foo1:  FAIL,  foo2:  SUCCESS"),

    test (hipCommand, examplesDir + "classic/classic1a.ss", " ", " foo1:  SUCCESS,  foo2:  SUCCESS"),

    test (hipCommand, examplesDir + "classic/classic1a.ss", " --classic", " foo1:  SUCCESS,  foo2:  FAIL"),

    test (hipCommand, examplesDir + "classic/classic2.ss", " ", " foo1:  FAIL,  foo2:  SUCCESS"),

    test (hipCommand, examplesDir + "classic/classic2a.ss", " ", " foo1:  SUCCESS,  foo2:  FAIL"),

    test (hipCommand, examplesDir + "classic/classic3.ss", " ", " foo1:  SUCCESS,  foo2:  SUCCESS"),

    test (hipCommand, examplesDir + "classic/classic3a.ss", " ", " foo1:  SUCCESS,  foo2:  SUCCESS"),

    test (hipCommand, examplesDir + "../../modular_examples/dll-modular.ss", " --overeps", " append:  SUCCESS,  append1:  SUCCESS,  append2:  SUCCESS, delete:  SUCCESS,  delete1:  SUCCESS,  f1:  SUCCESS, f2:  SUCCESS,  insert:  SUCCESS,  test_del:  SUCCESS, test_del2:  SUCCESS,  test_fold:  SUCCESS"),

    test (hipCommand, examplesDir + "../../modular_examples/selection-modular.ss", " --overeps --lda", " delete_min:  SUCCESS,  find_min:  SUCCESS,  selection_sort:  SUCCESS"),

    test (hipCommand, examplesDir + "../../modular_examples/qsort-modular.ss", " --overeps --lda", " append_bll:  SUCCESS,  partition:  SUCCESS,  qsort:  SUCCESS"),

    test (hipCommand, examplesDir + "vperm/vperm_check.ss", "  --ann-vp", " inc: SUCCESS, incCell: SUCCESS, test1: FAIL, test2: FAIL, test3: FAIL, test4: FAIL"),

    test (hipCommand, examplesDir + "vperm/task_decompose.ss", "  --ann-vp", " inc: SUCCESS, creator: SUCCESS, joiner: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/cell.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " test: SUCCESS, test1: FAIL"),

    test (hipCommand, examplesDir + "parahip/cell-extreme-cases.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " test: FAIL, test2: FAIL, test3: FAIL, test4: FAIL"),

    test (hipCommand, examplesDir + "parahip/ordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: SUCCESS, main: SUCCESS"),

    test (hipCommand, examplesDir + "parahip/unordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", " func: FAIL, main: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/hip-bperm1.ss", "  --en-para -perm bperm -tp redlog", "destroyCellFail: FAIL, readCell: SUCCESS, testNewCell: SUCCESS, testNewCell2: SUCCESS, updateCell: SUCCESS, updateCellFail: FAIL"),

    test (hipCommand, examplesDir + "veribsync/barrier-static-consistency.ss", "  --en-para -perm bperm -tp redlog", "main: SUCCESS, main_fail: FAIL, participant: SUCCESS, participant1: SUCCESS, participant_fail: FAIL"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp3.ss", "  --en-para -perm bperm -tp redlog", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS"),

    test (hipCommand, examplesDir + "veribsync/barrier-dynamic-exp4.ss", "  --en-para -perm bperm -tp redlog", "childthread1: SUCCESS, childthread2: SUCCESS, main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "conchip/mapreduce.ss", "  -tp parahip --classic", "count_helper: SUCCESS, countList: SUCCESS, destroyList: SUCCESS, main: SUCCESS, mapper_helper: SUCCESS, mapper: SUCCESS, reducer1: SUCCESS, reducer2: SUCCESS"),

    test (hipCommand, examplesDir + "conchip/multi-join2.ss", "  -tp parahip  -perm fperm --classic", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS"),

    test (hipCommand, examplesDir + "conchip/latch-exp2.ss", "  -tp parahip --classic", "main: FAIL, thread1: SUCCESS"),

    test (hipCommand, examplesDir + "conchip/deadpool.ss", "  -tp parahip -perm fperm --classic", "destroyDeadPool: SUCCESS, forkHelper: SUCCESS, forkThreads: SUCCESS, joinHelper: SUCCESS, joinThreads: SUCCESS, main: SUCCESS, thread: SUCCESS"))
  }

  def makeHipBarrTests() = {
    List(test (hipCommand, examplesDir + "../tree_shares/thesis/video_ex1_th3.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " th1_loop: SUCCESS, th1: SUCCESS, th2_loop: SUCCESS, th2: SUCCESS, th3_loop: SUCCESS, th3: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/SIMD_ex1_v2_th3.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " controll: SUCCESS, control: SUCCESS, thl1: SUCCESS, th1: SUCCESS, thl2: SUCCESS, th2: SUCCESS, thl3: SUCCESS, th3: SUCCESS, thl4: SUCCESS, th4: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/SIMD_ex1_th1.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " controll: SUCCESS, control: SUCCESS, thl1: SUCCESS, th1: SUCCESS, thl2: SUCCESS, th2: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/PIPE_ex1_th5.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " controll: SUCCESS, thl5: SUCCESS, thl1: SUCCESS, thl2: SUCCESS, thl3: SUCCESS, thl4: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/MIXED_ex1_th4.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " ctl12: SUCCESS, ctl11: SUCCESS, starterc: SUCCESS, thl12: SUCCESS, thl22: SUCCESS, thl21: SUCCESS, thl11: SUCCESS, startert1: SUCCESS, startert2: SUCCESS, thl32: SUCCESS, thl31: SUCCESS, startert3: SUCCESS, thl42: SUCCESS, thl41: SUCCESS, startert4: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/MISD_ex2_th5.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " controll: SUCCESS, control: SUCCESS, thl: SUCCESS, th: SUCCESS, thl2: SUCCESS, th2: SUCCESS, thl3: SUCCESS, th3: SUCCESS, thl4: SUCCESS, th4: SUCCESS, thl5: SUCCESS, th5: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/MISD_ex1_th5.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " controll: SUCCESS, control: SUCCESS, thl: SUCCESS, th: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/barrier-weak.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " th1_loop: SUCCESS, th1: SUCCESS, th2_loop: SUCCESS, th2: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/barrier-strong.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " th1_loop: SUCCESS, th1: SUCCESS, th2_loop: SUCCESS, th2: SUCCESS"),

    test (hipCommand, examplesDir + "../tree_shares/thesis/barrier-paper.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", " th1_loop: SUCCESS, th1: SUCCESS, th2_loop: SUCCESS, th2: SUCCESS"))
  }
}
