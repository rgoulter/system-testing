package edu.nus.systemtesting

import java.io.PrintWriter

import com.typesafe.config.Config

import edu.nus.systemtesting.testsuite.HipTestSuite

class HipTestSuiteUsage(configuration : Config) {
  val HIP_COMMAND = configuration.getString("HIP_COMMAND")
  val BASE_DIR = configuration.getString("HIP_DIR")
  val OUTPUT_DIR = configuration.getString("HIP_OUTPUT_DIRECTORY")

  def run() : Unit = {
    val suite = new HipTestSuite(new PrintWriter(System.out, true), configuration)

    addInfinityTests(suite)
    addArrayTests(suite)
    addListTests(suite)
    addTermTests(suite)
    addListTests(suite)
    addImmTests(suite)
    //    addThreadTests(suite)
    addHipBagaTests(suite)
    addMemTests(suite)
    addVeribsyncTests(suite)
    addParahipTests(suite)
    //    addVpermTests(suite)
    addHipTests(suite)
    //    addHipBarrTests(suite)
    suite addTest (HIP_COMMAND, BASE_DIR + "imm-field/sll.ss", "-tp oc --field-ann --etcsu1 ", OUTPUT_DIR, "sll.out", "delete: SUCCESS, get_tail: SUCCESS, insert: SUCCESS, insert2: SUCCESS")

    suite.runAllTests
    suite generateTestStatistics
  }

  def addInfinityTests(suite : HipTestSuite) : Unit = {
    // Infinity
    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/inflist.ss", "--dsd --en-inf", OUTPUT_DIR, "inflist.out", "remove: SUCCESS, append: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/infll_take.ss", "--dsd --en-inf", OUTPUT_DIR, "infll_take.out", "take: SUCCESS")

    //    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/inftree.ss", "--dsd --en-inf", OUTPUT_DIR, "inftree.out", "count: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/stream.ss", "--dsd --en-inf", OUTPUT_DIR, "stream.out", "zip: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/bst-inf.ss", "--dsd --en-inf", OUTPUT_DIR, "bst_inf.out", "delete: SUCCESS, remove_min: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/inf-selsort.ss", "--dsd --en-disj-compute --en-inf", OUTPUT_DIR, "inf_selsort.out", "find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/inf-ins.ss", "--dsd --en-inf", OUTPUT_DIR, "inf_ins.out", "insert: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/inf-sel.ss", "--dsd --en-inf", OUTPUT_DIR, "inf_sel.out", "find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/bubble-inf.ss", "--dsd --en-inf", OUTPUT_DIR, "bubble_inf.out", "id2: SUCCESS, id3: SUCCESS, bubble: SUCCESS, bsort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/heaps-inf.ss", "--en-inf", OUTPUT_DIR, "heaps_inf.out", "insert: SUCCESS, deleteoneel: SUCCESS, deleteone: SUCCESS, deletemax: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "infinity/merge-inf.ss", "--dsd --en-inf --en-disj-compute", OUTPUT_DIR, "merge_inf.out", "merge: SUCCESS")
  }

  def addArrayTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_at.java", "", OUTPUT_DIR, "arr_at.out", "main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_binarysearch.java", "", OUTPUT_DIR, "arr_binarysearch.out", "binary_search: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_search_decrease_less_than_two.java", "", OUTPUT_DIR, "arr_search_decrease_less_than_two.out", "searchzero: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_bubblesort.java", "", OUTPUT_DIR, "arr_bubblesort.out", "bubblesort: SUCCESS, bubble: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_bubblesort_perm.java", "", OUTPUT_DIR, "arr_bubblesort_perm.out", "bubblesort: SUCCESS, bubble: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_double.java", "", OUTPUT_DIR, "arr_double.out", "doublearr: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_extract_nonzeros.java", "", OUTPUT_DIR, "arr_extract_nonzeros.out", "copy_nonzeros: SUCCESS, count_nonzeros: SUCCESS, extract_nonzeros: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_init.java", "", OUTPUT_DIR, "arr_init.out", "zinit: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_insertsort.java", "", OUTPUT_DIR, "arr_insertsort.out", "insertelm: SUCCESS, insertion_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_insertsort_perm.java", "", OUTPUT_DIR, "arr_insertsort_perm.out", "insertelm: SUCCESS, insertion_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_invert.java", "", OUTPUT_DIR, "arr_invert.out", "Invert: SUCCESS, InvertHelper: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_max.java", "", OUTPUT_DIR, "arr_max.out", "max_value_of_array: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_mergesort.java", "", OUTPUT_DIR, "arr_mergesort.out", "merge_sorted_arrays: SUCCESS, copy_array: SUCCESS, merge_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_new_exp.java", "", OUTPUT_DIR, "arr_new_exp.out", "main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_nqueens.java", "", OUTPUT_DIR, "arr_nqueens.out", "nQueens: SUCCESS, nQueensHelper: SUCCESS, nQueensHelperHelper: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_qsort.java", "", OUTPUT_DIR, "arr_qsort.out", "arraypart: SUCCESS, qsort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_rev.java", "", OUTPUT_DIR, "arr_rev.out", "arrayrev: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_selectionsort.java", "", OUTPUT_DIR, "arr_selectionsort.out", "array_index_of_max: SUCCESS, selection_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_selectionsort_perm.java", "", OUTPUT_DIR, "arr_selectionsort_perm.out", "array_index_of_max: SUCCESS, selection_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_sparse.java", "--imm", OUTPUT_DIR, "arr_sparse.out", "create: SUCCESS, get: SUCCESS, setsa: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "array/arr_sum.java", "", OUTPUT_DIR, "arr_sum.out", "sigmaright: SUCCESS, sigmaleft: SUCCESS")
  }

  def addListTests(suite : HipTestSuite) : Unit = {
    // Lists
    suite addTest (HIP_COMMAND, BASE_DIR + "lists/demo.ss", " ", OUTPUT_DIR, "demo.out", ":  reverse,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/demo2.ss", " ", OUTPUT_DIR, "demo2.out", " app_rev:  SUCCESS,  reverse:  SUCCESS,  append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/err-coq.ss", " ", OUTPUT_DIR, "err_coq.out", " ret_first2:  SUCCESS,  ret_first:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll.ss", " ", OUTPUT_DIR, "ll.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll_bak.ss", " ", OUTPUT_DIR, "ll_bak.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll_bak2.ss", " ", OUTPUT_DIR, "ll_bak2.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll_bak3.ss", " ", OUTPUT_DIR, "ll_bak3.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll_nolists.ss", " ", OUTPUT_DIR, "ll_nolists.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll_test1.ss", " ", OUTPUT_DIR, "ll_test1.out", " reverse:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll_test2.ss", " ", OUTPUT_DIR, "ll_test2.out", " delete:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll_test4.ss", " ", OUTPUT_DIR, "ll_test4.out", " test:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/ll_test5.ss", " ", OUTPUT_DIR, "ll_test5.out", " delete_val:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/lrev-bug.ss", " ", OUTPUT_DIR, "lrev_bug.out", " lrev:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/lrev.ss", " ", OUTPUT_DIR, "lrev.out", " lrev:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/merge.ss", " ", OUTPUT_DIR, "merge.out", " merge:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/merge1.ss", " ", OUTPUT_DIR, "merge1.out", " merge:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/merge2.ss", " ", OUTPUT_DIR, "merge2.out", " merge:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/merge3.ss", " ", OUTPUT_DIR, "merge3.out", " merge:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/mk_zero.ss", " ", OUTPUT_DIR, "mk_zero.out", " mk_zero:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "lists/perm.ss", " ", OUTPUT_DIR, "perm.out", " append:  SUCCESS")
  }

  def addTermTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "term/e1.ss", " ", OUTPUT_DIR, "e1.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex1.ss", " ", OUTPUT_DIR, "ex1.out", " length:  SUCCESS,  app2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex10.ss", " ", OUTPUT_DIR, "ex10.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex11.ss", " ", OUTPUT_DIR, "ex11.out", " bsearch:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex15.ss", " ", OUTPUT_DIR, "ex15.out", " loop:  SUCCESS,  f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex16.ss", " ", OUTPUT_DIR, "ex16.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex2.ss", " ", OUTPUT_DIR, "ex2.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex3.ss", " ", OUTPUT_DIR, "ex3.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex4.ss", " ", OUTPUT_DIR, "ex4.out", " inc_loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex5.ss", " ", OUTPUT_DIR, "ex5.out", " foo:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex6.ss", " ", OUTPUT_DIR, "ex6.out", " Ack:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex7.ss", " ", OUTPUT_DIR, "ex7.out", " loop_aux1:  SUCCESS,  loop_aux:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex8.ss", " ", OUTPUT_DIR, "ex8.out", " loop2:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/ex9.ss", " ", OUTPUT_DIR, "ex9.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/mutual.ss", " ", OUTPUT_DIR, "mutual.out", " g:  SUCCESS,  f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/cav08-1.ss", " ", OUTPUT_DIR, "benchs_lit_cav08_1.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/cav08-2.ss", " ", OUTPUT_DIR, "benchs_lit_cav08_2.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/cav08-3.ss", " ", OUTPUT_DIR, "benchs_lit_cav08_3.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/cav08-4.ss", " ", OUTPUT_DIR, "benchs_lit_cav08_4.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/cav08-5.ss", " ", OUTPUT_DIR, "benchs_lit_cav08_5.out", " loop:  SUCCESS,  f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/cav08-6.ss", " ", OUTPUT_DIR, "benchs_lit_cav08_6.out", " gcd:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/dijkstra76-1.ss", " ", OUTPUT_DIR, "benchs_lit_dijkstra76_1.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/dijkstra76-2.ss", " ", OUTPUT_DIR, "benchs_lit_dijkstra76_2.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/dijkstra76-3.ss", " ", OUTPUT_DIR, "benchs_lit_dijkstra76_3.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/pldi06-1.ss", " ", OUTPUT_DIR, "benchs_lit_pldi06_1.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/pldi06-2.ss", " ", OUTPUT_DIR, "benchs_lit_pldi06_2.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/pldi06-3.ss", " ", OUTPUT_DIR, "benchs_lit_pldi06_3.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/pldi06-4.ss", " ", OUTPUT_DIR, "benchs_lit_pldi06_4.out", " main:  SUCCESS,  loop:  SUCCESS,  loop_aux:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/pldi06-5.ss", " ", OUTPUT_DIR, "benchs_lit_pldi06_5.out", " Ack:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/popl07-1.ss", " ", OUTPUT_DIR, "benchs_lit_popl07_1.out", " loop_1:  SUCCESS,  loop_2:  SUCCESS,  loop_3:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/popl07-2.ss", " ", OUTPUT_DIR, "benchs_lit_popl07_2.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/sas05.ss", " ", OUTPUT_DIR, "benchs_lit_sas05.out", " loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/sas10-1.ss", " ", OUTPUT_DIR, "benchs_lit_sas10_1.out", " f:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/sas10-2.ss", " ", OUTPUT_DIR, "benchs_lit_sas10_2.out", " foo:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/sas10-2a.ss", " ", OUTPUT_DIR, "benchs_lit_sas10_2a.out", " foo:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/sas10-3.ss", " ", OUTPUT_DIR, "benchs_lit_sas10_3.out", " main:  SUCCESS,  foo:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/vcc-1.ss", " ", OUTPUT_DIR, "benchs_lit_vcc_1.out", " f:  SUCCESS,  g:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/vmcai05-1a.ss", " ", OUTPUT_DIR, "benchs_lit_vmcai05_1a.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/lit/vmcai05-1b.ss", " -tp redlog", OUTPUT_DIR, "benchs_lit_vmcai05_1b.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/AlternatingIncr.ss", " ", OUTPUT_DIR, "benchs_key_AlternatingIncr.out", " increase:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/AlternDiv-invalid-1.ss", " -tp redlog", OUTPUT_DIR, "benchs_key_AlternDiv_invalid_1.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/AlternDiv.ss", " -tp redlog", OUTPUT_DIR, "benchs_key_AlternDiv.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/AlternDivWidening.ss", " -tp redlog", OUTPUT_DIR, "benchs_key_AlternDivWidening.out", " loop:  SUCCESS,  loop_aux:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/AlternDivWide.ss", " ", OUTPUT_DIR, "benchs_key_AlternDivWide.out", " loop:  SUCCESS,  loop_aux:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/AlternKonv.ss", " ", OUTPUT_DIR, "benchs_key_AlternKonv.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Collatz.ss", " ", OUTPUT_DIR, "benchs_key_Collatz.out", " collatz:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/ComplInterv2.ss", " ", OUTPUT_DIR, "benchs_key_ComplInterv2.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/ComplInterv3.ss", " ", OUTPUT_DIR, "benchs_key_ComplInterv3.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/ComplInterv.ss", " -tp z3", OUTPUT_DIR, "benchs_key_ComplInterv.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/ComplxStruc-may.ss", " ", OUTPUT_DIR, "benchs_key_ComplxStruc_may.out", " complxStruc:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/ComplxStruc2.ss", " ", OUTPUT_DIR, "benchs_key_ComplxStruc2.out", " loop:  SUCCESS,  complxStruc:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/ConvLower.ss", " ", OUTPUT_DIR, "benchs_key_ConvLower.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Cousot.ss", " ", OUTPUT_DIR, "benchs_key_Cousot.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/DoubleNeg.ss", " -tp redlog", OUTPUT_DIR, "benchs_key_DoubleNeg.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Even.ss", " ", OUTPUT_DIR, "benchs_key_Even.out", " even:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex01.ss", " ", OUTPUT_DIR, "benchs_key_Ex01.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex02.ss", " ", OUTPUT_DIR, "benchs_key_Ex02.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex03.ss", " ", OUTPUT_DIR, "benchs_key_Ex03.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex04.ss", " ", OUTPUT_DIR, "benchs_key_Ex04.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex05.ss", " ", OUTPUT_DIR, "benchs_key_Ex05.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex06.ss", " ", OUTPUT_DIR, "benchs_key_Ex06.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex07.ss", " ", OUTPUT_DIR, "benchs_key_Ex07.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex08.ss", " ", OUTPUT_DIR, "benchs_key_Ex08.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Ex09.ss", " ", OUTPUT_DIR, "benchs_key_Ex09.out", " half:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Fibonacci.ss", " ", OUTPUT_DIR, "benchs_key_Fibonacci.out", " fib:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Flip2.ss", " ", OUTPUT_DIR, "benchs_key_Flip2.out", " flip:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Flip3.ss", " ", OUTPUT_DIR, "benchs_key_Flip3.out", " flip:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Flip.ss", " ", OUTPUT_DIR, "benchs_key_Flip.out", " flip:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Gauss.ss", " ", OUTPUT_DIR, "benchs_key_Gauss.out", " sum:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Gcd-may.ss", " ", OUTPUT_DIR, "benchs_key_Gcd_may.out", " gcd:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Lcm.ss", " ", OUTPUT_DIR, "benchs_key_Lcm.out", " lcm:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Marbie1.ss", " ", OUTPUT_DIR, "benchs_key_Marbie1.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Marbie2.ss", " ", OUTPUT_DIR, "benchs_key_Marbie2.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Middle.ss", " ", OUTPUT_DIR, "benchs_key_Middle.out", " middle:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/MirrorIntervSim.ss", " ", OUTPUT_DIR, "benchs_key_MirrorIntervSim.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/MirrorInterv.ss", " ", OUTPUT_DIR, "benchs_key_MirrorInterv.out", " mirrorInterv:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/ModuloLower.ss", " ", OUTPUT_DIR, "benchs_key_ModuloLower.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/ModuloUp.ss", " -tp redlog", OUTPUT_DIR, "benchs_key_ModuloUp.out", " up:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Narrowing.ss", " ", OUTPUT_DIR, "benchs_key_Narrowing.out", " narrowing:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/NarrowKonv.ss", " ", OUTPUT_DIR, "benchs_key_NarrowKonv.out", " narrowKonv:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/NegPos.ss", " -tp redlog", OUTPUT_DIR, "benchs_key_NegPos.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Plait-may.ss", " ", OUTPUT_DIR, "benchs_key_Plait_may.out", " plait:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/Sunset.ss", " ", OUTPUT_DIR, "benchs_key_Sunset.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/TrueDiv.ss", " ", OUTPUT_DIR, "benchs_key_TrueDiv.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/TwoFloatInterv.ss", " ", OUTPUT_DIR, "benchs_key_TwoFloatInterv.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/UpAndDownIneq.ss", " ", OUTPUT_DIR, "benchs_key_UpAndDownIneq.out", " upAndDown:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/UpAndDown.ss", " ", OUTPUT_DIR, "benchs_key_UpAndDown.out", " upAndDown:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileBreak.ss", " ", OUTPUT_DIR, "benchs_key_WhileBreak.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileDecr.ss", " ", OUTPUT_DIR, "benchs_key_WhileDecr.out", " decrease:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileIncrPart.ss", " ", OUTPUT_DIR, "benchs_key_WhileIncrPart.out", " increase:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileIncr.ss", " ", OUTPUT_DIR, "benchs_key_WhileIncr.out", " increase:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileNestedOffset.ss", " ", OUTPUT_DIR, "benchs_key_WhileNestedOffset.out", " increase:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileNested.ss", " ", OUTPUT_DIR, "benchs_key_WhileNested.out", " increase:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhilePart.ss", " ", OUTPUT_DIR, "benchs_key_WhilePart.out", " increase:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileSingle.ss", " ", OUTPUT_DIR, "benchs_key_WhileSingle.out", " increase:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileSum.ss", " ", OUTPUT_DIR, "benchs_key_WhileSum.out", " increase:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/key/WhileTrue.ss", " ", OUTPUT_DIR, "benchs_key_WhileTrue.out", " endless:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/DivMinus2.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_DivMinus2.out", " main:  SUCCESS,  div:  SUCCESS,  minus:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/DivMinus.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_DivMinus.out", " main:  SUCCESS,  div:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/DivWithoutMinus.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_DivWithoutMinus.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/Duplicate.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_Duplicate.out", " main:  SUCCESS,  round:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/GCD2.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Aprove_09_GCD2.out", " main:  SUCCESS,  gcd:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/GCD3.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_GCD3.out", " main:  SUCCESS,  gcd:  SUCCESS,  mod:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/GCD4.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_GCD4.out", " main:  SUCCESS,  gcd:  SUCCESS,  mod:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/GCD5.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Aprove_09_GCD5.out", " main:  SUCCESS,  gcd:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/GCD.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Aprove_09_GCD.out", " main:  SUCCESS,  gcd:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/LogAG.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_LogAG.out", " main:  SUCCESS,  half:  SUCCESS,  log:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/LogBuiltIn.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_LogBuiltIn.out", " main:  SUCCESS,  log:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/LogIterative.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Aprove_09_LogIterative.out", " main:  SUCCESS,  log:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/LogMult.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Aprove_09_LogMult.out", " main:  SUCCESS,  log:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/Log.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_Log.out", " main:  SUCCESS,  half:  SUCCESS,  log:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/McCarthyIterative-may.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_McCarthyIterative_may.out", " mcCarthy:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/McCarthyRec.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_McCarthyRec.out", " mcCarthy:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/MinusBuiltIn.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_MinusBuiltIn.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/MinusMin.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_MinusMin.out", " main:  SUCCESS,  mn:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/MinusUserDefined.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_MinusUserDefined.out", " main:  SUCCESS,  gt:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/Mod.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_Mod.out", " main:  SUCCESS,  mod:  SUCCESS,  minus:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/PlusSwap.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_PlusSwap.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Aprove_09/Round3.ss", " ", OUTPUT_DIR, "benchs_aprove_Aprove_09_Round3.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/AProVE_10/AG313.ss", " ", OUTPUT_DIR, "benchs_aprove_AProVE_10_AG313.out", " main:  SUCCESS,  quot:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/AProVE_11_iterative/RetValRec.ss", " ", OUTPUT_DIR, "benchs_aprove_AProVE_11_iterative_RetValRec.out", " main:  SUCCESS,  ret:  SUCCESS,  test:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/AProVE_11_iterative/RetVal.ss", " ", OUTPUT_DIR, "benchs_aprove_AProVE_11_iterative_RetVal.out", " main:  SUCCESS,  ret:  SUCCESS,  test:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/AProVE11NO/LoopingNonTerm.ss", " ", OUTPUT_DIR, "benchs_aprove_AProVE11NO_LoopingNonTerm.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/AProVE11NO/NonPeriodicNonTerm2.ss", " ", OUTPUT_DIR, "benchs_aprove_AProVE11NO_NonPeriodicNonTerm2.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/Avg.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_Avg.out", " avg:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/EqUserDefRec.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_EqUserDefRec.out", " main:  SUCCESS,  eq:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/Fibonacci.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_Fibonacci.out", " main:  SUCCESS,  fib:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/LeUserDefRec.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_LeUserDefRec.out", " main:  SUCCESS,  le:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/LogRecursive.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_LogRecursive.out", " main:  SUCCESS,  log:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/Nest.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_Nest.out", " main:  SUCCESS,  nest:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec01.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_TerminatiorRec01.out", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec02.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_TerminatiorRec02.out", " fact:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec03.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_TerminatiorRec03.out", " f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec04-modified.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_TerminatiorRec04_modified.out", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/TerminatiorRec04.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_TerminatiorRec04.out", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/TimesPlusUserDef.ss", " ", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_TimesPlusUserDef.out", " main:  SUCCESS,  times:  SUCCESS,  plus:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/BOG_RTA_11/TwoWay.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_BOG_RTA_11_TwoWay.out", " twoWay:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Break.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Break.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Continue1.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Continue1.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Continue.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Continue.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/costa09-example_1.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_costa09_example_1.out", " incr:  SUCCESS,  add:  SUCCESS,  incr2:  SUCCESS,  add2:  SUCCESS,  incr3:  SUCCESS,  add3:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/costa09-example_2.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_costa09_example_2.out", " main:  SUCCESS,  divBy:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/costa09-example_3.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_costa09_example_3.out", " main:  SUCCESS,  m:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc1-exc.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc1_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc2-exc.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc2_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc3-exc.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc3_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc4-exc.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc4_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc5-exc.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc5_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc-exc.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc1-no.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc1_no.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc2-no.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc2_no.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc3-no.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc3_no.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc4-no.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc4_no.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc5-no.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc5_no.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Exc-no.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Exc_no.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Loop1.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Loop1.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Nested.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Nested.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/Sequence.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_Sequence.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09/TestJulia4.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_TestJulia4.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Ackermann.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Ackermann.out", " main:  SUCCESS,  ack:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Double-1.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Double_1.out", " test:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Double2-1.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Double2_1.out", " main:  SUCCESS,  test:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Double2.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Double2.out", " main:  SUCCESS,  test:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Double3-1.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Double3_1.out", " main:  SUCCESS,  test:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Double3.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Double3.out", " main:  SUCCESS,  test:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Double.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Double.out", " main:  SUCCESS,  test:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Factorial.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Factorial.out", " main:  SUCCESS,  fact:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/FactSumList.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_FactSumList.out", " doSum:  SUCCESS,  fact:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/FactSum.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_FactSum.out", " doSum:  SUCCESS,  fact:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Costa_Julia_09-recursive/Hanoi.ss", " ", OUTPUT_DIR, "benchs_aprove_Costa_Julia_09_recursive_Hanoi.out", " main:  SUCCESS,  sol:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Iterative/NonPeriodic.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_10_Iterative_NonPeriodic.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Iterative/Test11.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Julia_10_Iterative_Test11.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Iterative/Test2.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_10_Iterative_Test2.out", " main:  SUCCESS,  iter:  SUCCESS,  add:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/AckR.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_AckR.out", " main:  SUCCESS,  ack:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/FibSLR.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_FibSLR.out", " main:  SUCCESS,  fib:  SUCCESS,  doSum:  SUCCESS,  create:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/HanR.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_HanR.out", " main:  SUCCESS,  sol:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/Power.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_Power.out", " power:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/EvenOdd.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_EvenOdd.out", " even:  SUCCESS,  odd:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/Recursions.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_Recursions.out", " main:  SUCCESS,  rec0:  SUCCESS,  rec1:  SUCCESS, rec2:  SUCCESS,  rec3:  SUCCESS,  rec4:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/Test10.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_Test10.out", " main:  SUCCESS,  rec:  SUCCESS,  test:  SUCCESS,  descend:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/Test12.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_Test12.out", " main:  SUCCESS,  rec:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_10_Recursive/Test1.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_10_Recursive_Test1.out", " main:  SUCCESS,  rec:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/ChooseLife.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_ChooseLife.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/Choose.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_Choose.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/Continue.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_Continue.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/Loop.ss", " -tp redlog", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_Loop.out", " main:  SUCCESS,  test:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_00.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_00.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_01.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_01.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_02.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_02.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_03.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_03.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_04.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_04.out", " main:  SUCCESS,  for_1:  SUCCESS,  for_2:  SUCCESS,  for_3:  SUCCESS,  for_4:  SUCCESS,  for_5:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_05.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_05.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_06.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_06.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_10.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_10.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_11.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_11.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_12.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_12.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_20.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_20.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_21.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_21.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_22.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_22.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_23.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_23.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/NO_24.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_NO_24.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/Parts.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_Parts.out", " parts:  SUCCESS,  main:  SUCCESS,  for_1:  SUCCESS,  loop_1:  SUCCESS,  for_2:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/Julia_11_iterative/Swingers.ss", " ", OUTPUT_DIR, "benchs_aprove_Julia_11_iterative_Swingers.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaA10.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaA10.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaA1.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaA1.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaA4.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaA4.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaA5.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaA5.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaA6.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaA6.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaA7.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaA7.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaA8.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaA8.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaA9.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaA9.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB10.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB10.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB11.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB11.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB12.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB12.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB13.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB13.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB14.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB14.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB15.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB15.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB16-loop.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB16_loop.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB16.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB16.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB17.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB17.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB18.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB18.out", " main:  SUCCESS,  loop:  SUCCESS,  decrease:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB1.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB1.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB2.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB2.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB3.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB3.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB4.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB4.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB4-loop.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB4_loop.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB5.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB5.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB6.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB6.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB7.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB7.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaB8.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaB8.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC10-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC10_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC11.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC11.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC11-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC11_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC1.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC1.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC1-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC1_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC2-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC2_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC3.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC3.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC3-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC3_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC4-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC4_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC5-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC5_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC7-simpl-1.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC7_simpl_1.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC7-simpl-2.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC7_simpl_2.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC7-simpl.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC7_simpl.out", " loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC7.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC7.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC7-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC7_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC8-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC8_while.out", " main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "term/benchs/aprove/pasta/PastaC9-while.ss", " ", OUTPUT_DIR, "benchs_aprove_pasta_PastaC9_while.out", " main:  SUCCESS")
  }

  def addImmTests(suite: HipTestSuite): Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "imm/bigint.ss", "   --imm -tp redlog", OUTPUT_DIR, "bigint.out", " clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/bigint_imm.ss", "   --imm -tp redlog", OUTPUT_DIR, "bigint_imm.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, test:  SUCCESS,  add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult2:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/bigint_imm-star.ss", "   --imm -tp redlog", OUTPUT_DIR, "bigint_imm_star.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/bigint-tight.ss", "   --imm -tp redlog", OUTPUT_DIR, "bigint_tight.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/bigint-tight-imm.ss", "   --imm -tp redlog", OUTPUT_DIR, "bigint_tight_imm.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, test:  SUCCESS,  add_c:  SUCCESS, add:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/bigint-tight-imm-star.ss", "   --imm -tp redlog", OUTPUT_DIR, "bigint_tight_imm_star.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/append_imm.ss", "   --imm ", OUTPUT_DIR, "append_imm.out", " append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/kara.ss", "   --imm -tp redlog", OUTPUT_DIR, "kara.out", " karatsuba_mult: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/kara-imm-star.ss", "   --imm -tp redlog ", OUTPUT_DIR, "kara_imm_star.out", " karatsuba_mult: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/kara-imm-conj.ss", "  --imm -tp redlog", OUTPUT_DIR, "kara_imm_conj.out", " karatsuba_mult: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm/ll_imm.ss", "   --imm ", OUTPUT_DIR, "ll_imm.out", " length:  SUCCESS, append:  SUCCESS, sumN:  SUCCESS, set_next:  SUCCESS, get_next_next:  SUCCESS, get_next:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm-field/imspd.ss", "-tp oc --field-ann --etcsu1 ", OUTPUT_DIR, "imspd.out", "check_pass: SUCCESS, login: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm-field/getset.ss", "-tp oc --field-ann --etcsu1 ", OUTPUT_DIR, "getset.out", "sset: SUCCESS, get: SUCCESS, setA: SUCCESS, getA: SUCCESS, non_negative: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm-field/bigint.ss", "-tp redlog --field-ann --etcsu1 ", OUTPUT_DIR, "bigint.out", "clone: SUCCESS, add_one_digit: SUCCESS, add_c: SUCCESS, add: SUCCESS, div_with_remainder: SUCCESS, bigint_of: SUCCESS, compare_int: SUCCESS, is_zero: SUCCESS, compare2: SUCCESS, int_value: SUCCESS, mult_c: SUCCESS, shift_left: SUCCESS, mult: SUCCESS, sub_one_digit: SUCCESS, sub_c: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm-field/insertion_simple.ss", "-tp oc --field-ann --etcsu1 ", OUTPUT_DIR, "insertion_simple.out", "insert: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "imm-field/schorr-waite-list.ss", "-tp om --field-ann --etcsu1 ", OUTPUT_DIR, "schorr_waite_list.out", "lscan: SUCCESS")
  }

  def addThreadTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "threads/motiv-example.ss", "  --en-para --en-thrd-resource -tp redlog", OUTPUT_DIR, "threads_motiv_example.out", " main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/motiv-example2.ss", "  --en-para --en-thrd-resource -tp redlog", OUTPUT_DIR, "threads_motiv_example2.out", " main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/no-deadlock-nonlexical2.ss", "  --en-para --en-thrd-resource -tp parahip --en-lsmu-infer", OUTPUT_DIR, "threads_no_deadlock_nonlexical2.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/no-deadlock-nonlexical.ss", "  --en-para --en-thrd-resource -tp parahip --en-lsmu-infer", OUTPUT_DIR, "threads_no_deadlock_nonlexical.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/forkjoin.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_forkjoin.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/cell4.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_cell4.out", " inc: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/ls-bind.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_ls_bind.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_no_deadlock1.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_no_deadlock2.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_no_deadlock3.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_deadlock1.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_deadlock2.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_deadlock3.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/disj-no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_disj_no_deadlock1.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/disj-no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_disj_no_deadlock2.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/disj-no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_disj_no_deadlock3.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/disj-deadlock.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_disj_deadlock.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/ordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_ordered_locking.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/unordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_unordered_locking.out", " func: FAIL, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/oracle.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_oracle.out", " thread: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/owicki-gries.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_owicki_gries.out", " incrementor2: SUCCESS, incrementor1: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "threads/fibonacci.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", OUTPUT_DIR, "threads_fibonacci.out", " seq_fib: SUCCESS, para_fib: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "trees.ss", "", OUTPUT_DIR, "trees.out", "insert: SUCCESS")
  }

  def addBagsTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-all-1.ss", " ", OUTPUT_DIR, "avl_all_1.out", " remove_min:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-all.ss", " ", OUTPUT_DIR, "avl_all.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS, remove_max_add:  SUCCESS,  : remove_min_add, insert:  SUCCESS,  rotate_double_left:   SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-modular-2.ss", " ", OUTPUT_DIR, "avl_modular_2.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-modular-3.ss", " ", OUTPUT_DIR, "avl_modular_3.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS, remove_max_add:  SUCCESS,  : remove_min_add, insert:  SUCCESS,  rotate_double_left:   SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-modular-2.ss", " ", OUTPUT_DIR, "avl_modular_2.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS, height: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-modular-hei.ss", " ", OUTPUT_DIR, "avl_modular_hei.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  rotate_right2:  SUCCESS,  rotate_left2:  SUCCESS,  height: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-modular-new3.ss", " ", OUTPUT_DIR, "avl_modular_new3.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  is_mem:  SUCCESS, rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS,  height: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-modular-set.ss", "", OUTPUT_DIR, "avl_modular_set.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-modular-siz.ss", " ", OUTPUT_DIR, "avl_modular_siz.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl-modular.ss", " ", OUTPUT_DIR, "avl_modular.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl.scp.ss", "", OUTPUT_DIR, "avl.scp.out", " delete:  SUCCESS,  remove_min:  SUCCESS,  insert_inline1:  SUCCESS,  insert_inline:  SUCCESS,  insert1:  SUCCESS,  insert:  SUCCESS, build_avl2:  SUCCESS,  build_avl1:  SUCCESS,  rotate_double_right1:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left1:  SUCCESS, rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right1:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left1:  SUCCESS,  rotate_left:  SUCCESS, height1:  SUCCESS,  height:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/avl.ss", " ", OUTPUT_DIR, "avl.out", " insert_inline:  SUCCESS,   insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS, height:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/bubble.ss", " ", OUTPUT_DIR, "bubble.out", " bsort1:  SUCCESS,  bubble1:  SUCCESS,  id1:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/cll.ss", " ", OUTPUT_DIR, "cll.out", " delete2:  SUCCESS,  delete:  SUCCESS,  count:  SUCCESS,  count_rest:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/dll.ss", " ", OUTPUT_DIR, "dll.out", " append:  SUCCESS,  insert:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/insertion.ss", " ", OUTPUT_DIR, "insertion.out", " insertion_sort:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/ll.ss", " ", OUTPUT_DIR, "ll.out", " reverse1:  SUCCESS,  delete1:  SUCCESS,  insert:  SUCCESS,  append:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/merge-modular.ss", " ", OUTPUT_DIR, "merge_modular.out", " insert1:  SUCCESS,  merge1:  SUCCESS,  merge_sort1:  SUCCESS,  split1:  SUCCESS,  count1:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/merge.ss", " ", OUTPUT_DIR, "merge.out", " insert1:  SUCCESS,  merge1:  SUCCESS,  merge_sort1:  SUCCESS,  split1:  SUCCESS,  count1:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/qsort.ss", " ", OUTPUT_DIR, "qsort.out", " qsort1:  SUCCESS,  append_bll1:  SUCCESS,  partition1:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/rb_bags.ss", " ", OUTPUT_DIR, "rb_bags.out", " insert_1:  SUCCESS,  del_1:  SUCCESS,  remove_min_1:  SUCCESS,  del_2r_1:  SUCCESS,  del_2_1:  SUCCESS,  del_3r_1:  SUCCESS, del_3_1:  SUCCESS,  del_4r_1:  SUCCESS,  del_4_1:  SUCCESS,  del_5r_1:  SUCCESS,  del_5_1:  SUCCESS,  del_6r_1:  SUCCESS,  del_6_1:  SUCCESS, is_black_1:  SUCCESS,  is_red_1:  SUCCESS,  case_2r_1:  SUCCESS,  rotate_case_3r_1:  SUCCESS,  case_2_1:  SUCCESS,  rotate_case_3_1:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/rb.scp.ss", " ", OUTPUT_DIR, "rb.scp.out", " insert_1:  SUCCESS,  insert:  SUCCESS,  del_1:  SUCCESS,  del:  SUCCESS,  remove_min_1:  SUCCESS,  remove_min:  SUCCESS,  del_2r_1:  SUCCESS,  del_2r:  SUCCESS,  del_2_1:  SUCCESS,  del_2:  SUCCESS,  del_3r_1:  SUCCESS,  del_3r:  SUCCESS,  del_3_1:  SUCCESS,  del_3:  SUCCESS,  del_4r_1:  SUCCESS,  del_4r:  SUCCESS,  del_4_1:  SUCCESS,  del_4:  SUCCESS,  del_5r_1:  SUCCESS,  del_5r:  SUCCESS,  del_5_1:  SUCCESS,  del_5:  SUCCESS,  del_6r_1:  SUCCESS,  del_6r:  SUCCESS,  del_6_1:  SUCCESS,  del_6:  SUCCESS,  is_black_1:  SUCCESS,  is_black:  SUCCESS,  is_red_1:  SUCCESS,  is_red:  SUCCESS,  case_2r_1:  SUCCESS,  case_2r:  SUCCESS,  rotate_case_3r_1:  SUCCESS,  rotate_case_3r:  SUCCESS,  case_2_1:  SUCCESS,  case_2:  SUCCESS,  rotate_case_3_1:  SUCCESS,  rotate_case_3:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/selection.ss", " ", OUTPUT_DIR, "selection.out", " selection_sort:  SUCCESS,  delete_min:  SUCCESS,  find_min:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bags/trees.ss", " ", OUTPUT_DIR, "trees.out", " delete1:  SUCCESS,  remove_min1:  SUCCESS,  insert1:  SUCCESS,  flatten1:  SUCCESS,  append1:  SUCCESS")
  }

  def addHipBagaTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/eps.ss", " --baga-xpure", OUTPUT_DIR, "eps.out", " get_next: SUCCESS, get_next_next: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/append.ss", " --baga-xpure", OUTPUT_DIR, "append.out", " append: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/append-tail.ss", " --baga-xpure", OUTPUT_DIR, "append_tail.out", "append: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-bind.ss", " --baga-xpure", OUTPUT_DIR, "avl_bind.out", " height: SUCCESS,  rotate_left: SUCCESS,  rotate_right: SUCCESS,  get_max: SUCCESS,  rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl.ss", " --baga-xpure", OUTPUT_DIR, "avl.out", " height: SUCCESS, rotate_left: SUCCESS, rotate_right: SUCCESS, get_max: SUCCESS, rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS, insert_inline: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-orig-2.ss", " --baga-xpure", OUTPUT_DIR, "avl_orig_2.out", "height: SUCCESS, get_max: SUCCESS, insert: SUCCESS, double_left_child: SUCCESS, double_right_child: SUCCESS, rotate_left_child: SUCCESS,  rotate_right_child: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-orig3.ss", " --baga-xpure", OUTPUT_DIR, "avl_orig3.out", " height: SUCCESS, get_max: SUCCESS, insert: SUCCESS,  double_left_child: SUCCESS, double_right_child: SUCCESS,  rotate_left_child: SUCCESS, rotate_right_child: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/bll.ss", " --baga-xpure", OUTPUT_DIR, "bll.out", " insert: SUCCESS, delete: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/bubble.ss", " --baga-xpure", OUTPUT_DIR, "bubble.out", " id2: SUCCESS, id3: SUCCESS, bubble: SUCCESS, bsort: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/cll.ss", " --baga-xpure", OUTPUT_DIR, "cll.out", " test: SUCCESS, insert: SUCCESS, count_rest: SUCCESS, count: SUCCESS, delete: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/complete.ss", " --baga-xpure", OUTPUT_DIR, "complete.out", " maxim: SUCCESS, minim: SUCCESS, height: SUCCESS, min_height: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/dll.ss", " --baga-xpure", OUTPUT_DIR, "dll.out", " insert: SUCCESS, delete: SUCCESS, delete1: SUCCESS, test_del: SUCCESS, test_del2: SUCCESS, test_fold: SUCCESS, append: SUCCESS, append1: SUCCESS, f1: SUCCESS, f2: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/heaps.ss", " --baga-xpure", OUTPUT_DIR, "heaps.out", " insert: SUCCESS, deleteoneel: SUCCESS, deleteone: SUCCESS, ripple: SUCCESS, deletemax: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/insertion.ss", " --baga-xpure", OUTPUT_DIR, "insertion.out", " insert: SUCCESS, insertion_sort: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/ll.ss", " --baga-xpure", OUTPUT_DIR, "ll.out", " append: SUCCESS, ret_first: SUCCESS, get_next: SUCCESS, set_next: SUCCESS, set_null: SUCCESS, get_next_next: SUCCESS, insert: SUCCESS, delete: SUCCESS, create_list: SUCCESS, reverse: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/merge.ss", " --baga-xpure", OUTPUT_DIR, "merge.out", " count: SUCCESS, split_func: SUCCESS, merge_sort: SUCCESS, merge: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/perfect.ss", " --baga-xpure", OUTPUT_DIR, "perfect.out", " simple_insert: SUCCESS, create: SUCCESS, maxim: SUCCESS, height: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/qsort.ss", " --baga-xpure", OUTPUT_DIR, "qsort.out", " partition: SUCCESS, append_bll: SUCCESS, qsort: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/selection.ss", " --baga-xpure", OUTPUT_DIR, "selection.out", " find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/sll.ss", " --baga-xpure", OUTPUT_DIR, "sll.out", " insert: SUCCESS, insert2: SUCCESS, delete: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/trees.ss", " --baga-xpure", OUTPUT_DIR, "trees.out", " append: SUCCESS, count: SUCCESS, flatten: SUCCESS, insert: SUCCESS, remove_min: SUCCESS, delete: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/rb.ss", " --baga-xpure", OUTPUT_DIR, "rb.out", " rotate_case_3: SUCCESS, case_2: SUCCESS, rotate_case_3r: SUCCESS, case_2r: SUCCESS, is_red: SUCCESS, is_black: SUCCESS, del_6: SUCCESS, del_6r: SUCCESS, del_5: SUCCESS, del_5r: SUCCESS, del_4: SUCCESS, del_4r: SUCCESS, del_3: SUCCESS, del_3r: SUCCESS, del_2: SUCCESS, remove_min: SUCCESS, del: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/global-ll.ss", " --baga-xpure", OUTPUT_DIR, "global_ll.out", " insert_rec: SUCCESS, delete_last_rec: SUCCESS, insert: SUCCESS, delete_last: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/dll-modular.ss", " --overeps --baga-xpure", OUTPUT_DIR, "modular_examples_dll_modular.out", "append:  SUCCESS,  append1:  SUCCESS,  append2:  SUCCESS, delete:  SUCCESS,  delete1:  SUCCESS,  f1:  SUCCESS, f2:  SUCCESS,  insert:  SUCCESS,  test_del:  SUCCESS, test_del2:  SUCCESS,  test_fold:  SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/selection-modular.ss", " --overeps --lda --baga-xpure", OUTPUT_DIR, "modular_examples_selection_modular.out", "delete_min:  SUCCESS,  find_min:  SUCCESS,  selection_sort:  SUCCESS")

    suite addTest (HIP_COMMAND, "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/qsort-modular.ss", " --overeps --lda --baga-xpure", OUTPUT_DIR, "modular_examples_qsort_modular.out", "append_bll:  SUCCESS,  partition:  SUCCESS,  qsort:  SUCCESS")
  }

  def addMemTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "mem/dag.ss", "-tp om --mem --ramify", OUTPUT_DIR, "dag.out", "mark: SUCCESS, mark2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/dag_values.ss", "-tp om --mem --ramify", OUTPUT_DIR, "dag_values.out", "mark: SUCCESS, mark2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/dag_values_infer.ss", "-tp om --mem --ramify --infer-mem", OUTPUT_DIR, "dag_values_infer.out", "mark: SUCCESS, mark2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/graph.ss", "-tp om --mem --ramify", OUTPUT_DIR, "graph.out", "mark: SUCCESS, mark2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/graph_values.ss", "-tp om --mem --ramify", OUTPUT_DIR, "graph_values.out", "mark: SUCCESS, mark2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/graph_values_infer.ss", "-tp om --mem --ramify --infer-mem", OUTPUT_DIR, "graph_values_infer.out", "mark: SUCCESS, mark2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/dag_1.ss", "-tp om --mem --ramify", OUTPUT_DIR, "dag_1.out", "mark: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/dag_1_ramify.ss", "-tp om --mem --ramify", OUTPUT_DIR, "dag_1_ramify.out", "mark: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/graph_1.ss", "-tp om --mem --ramify", OUTPUT_DIR, "graph_1.out", "mark: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/graph_1_ramify.ss", "-tp om --mem --ramify", OUTPUT_DIR, "graph_1_ramify.out", "mark: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/dag_copy.ss", "-tp om --mem --ramify", OUTPUT_DIR, "dag_copy.out", "copy_dag: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/garbage_collector.ss", "-tp om --mem --ramify", OUTPUT_DIR, "garbage_collector.out", "mark: SUCCESS, sweep: SUCCESS, collect: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/garbage_collector_values.ss", "-tp om --mem --ramify", OUTPUT_DIR, "garbage_collector_values.out", "mark: SUCCESS, sweep: SUCCESS, collect: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/garbage_collector_values_infer.ss", "-tp om --mem --ramify --infer-mem", OUTPUT_DIR, "garbage_collector_values_infer.out", "mark: SUCCESS, sweep: SUCCESS, collect: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/llsortll.ss", "-tp om --mem --eps", OUTPUT_DIR, "llsortll.out", "overlaid_insert: SUCCESS, delete2: SUCCESS, insert2: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/infer_llsortll.ss", "-tp om --mem --eps --infer-mem", OUTPUT_DIR, "infer_llsortll.out", "overlaid_insert: SUCCESS, delete2: SUCCESS, insert2: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/lltree.ss", "-tp om --mem", OUTPUT_DIR, "lltree.out", "move_request: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/infer_lltree.ss", "-tp om --mem --infer-mem", OUTPUT_DIR, "infer_lltree.out", "move_request: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/lltree2.ss", "-tp om --mem --eps", OUTPUT_DIR, "lltree2.out", "move_request: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/infer_lltree2.ss", "-tp om --mem --eps --infer-mem", OUTPUT_DIR, "infer_lltree2.out", "move_request: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/nodell.ss", "-tp om --mem --ramify", OUTPUT_DIR, "nodell.out", "delete_cache: SUCCESS, delete: SUCCESS, add_L: SUCCESS, caching: SUCCESS, add_in: SUCCESS, find: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/nodell_infer.ss", "-tp om --mem --ramify --infer-mem", OUTPUT_DIR, "nodell_infer.out", "delete_cache: SUCCESS, delete: SUCCESS, add_L: SUCCESS, caching: SUCCESS, add_in: SUCCESS, find: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/pll.ss", "-tp om --mem", OUTPUT_DIR, "pll.out", "length: SUCCESS, sum: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/pll_ramify.ss", "-tp om --mem --ramify", OUTPUT_DIR, "pll_ramify.out", "length: SUCCESS, sum: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/pll_infer.ss", "-tp om --mem --infer-mem", OUTPUT_DIR, "pll_infer.out", "length: SUCCESS, sum: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/pll_ramify_infer.ss", "-tp om --mem --ramify --infer-mem", OUTPUT_DIR, "pll_ramify_infer.out", "length: SUCCESS, sum: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/doubly_circular_list.ss", "-tp om --mem", OUTPUT_DIR, "doubly_circular_list.out", "insert_node_dcll: SUCCESS, insert_lln: SUCCESS, insert_llt: SUCCESS, insert_dcll: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/infer_doubly_circular_list.ss", "-tp om --mem --infer-mem", OUTPUT_DIR, "infer_doubly_circular_list.out", "insert_node_dcll: SUCCESS, insert_lln: SUCCESS, insert_llt: SUCCESS, insert_dcll: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/process_schedular.ss", "-tp om --mem", OUTPUT_DIR, "process_schedular.out", "insert_process: SUCCESS, insert_rll: SUCCESS, insert_pll: SUCCESS, insert_sll: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/process_schedular_ramify.ss", "-tp om --mem --ramify", OUTPUT_DIR, "process_schedular_ramify.out", "insert_process: SUCCESS, insert_rll: SUCCESS, insert_pll: SUCCESS, insert_sll: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/ramified-cells.ss", "--mem --ramify", OUTPUT_DIR, "ramified_cells.out", "ex0: SUCCESS, ex1: SUCCESS, mark: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/ramified-pairs.ss", "--mem --ramify", OUTPUT_DIR, "ramified_pairs.out", "mark: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/jsvarstore.ss", "--mem --ramify", OUTPUT_DIR, "jsvarstore.out", "ex1: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/graph_spanning.ss", "-tp om --mem --ramify --eps", OUTPUT_DIR, "graph_spanning.out", "spanning: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "mem/graph_spanning_infer.ss", "-tp om --mem --ramify --eps --infer-mem", OUTPUT_DIR, "graph_spanning_infer.out", "spanning: SUCCESS")
  }

  def addVeribsyncTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/while-loop.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "while_loop.out", "fun: SUCCESS, fun3: SUCCESS, loop_fun: SUCCESS, loop_fun3: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/while-loop2.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "while_loop2.out", "fun: SUCCESS, fun3: SUCCESS, fun4: SUCCESS, fun5: SUCCESS, fun6: SUCCESS, fun7: SUCCESS, fun8: SUCCESS, fun9: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/hip-bperm1.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "hip_bperm1.out", "destroyCellFail: FAIL, readCell: SUCCESS, testNewCell: SUCCESS, testNewCell2: SUCCESS, updateCell: SUCCESS, updateCellFail: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/bperm-exp.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "bperm_exp.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-primitives.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_primitives.out", "main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-exp1.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_exp1.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-exp2.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_exp2.out", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-exp3.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_exp3.out", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-complex.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_complex.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-complex2.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_complex2.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-complex3.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_complex3.out", "main: SUCCESS, thread: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-multiple.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_multiple.out", "main: SUCCESS, main_fail: SUCCESS, participant: SUCCESS, participant_fail: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-consistency.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_static_consistency.out", "main: SUCCESS, main_fail: FAIL, participant: SUCCESS, participant1: SUCCESS, participant_fail: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp1.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_dynamic_exp1.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp2.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_dynamic_exp2.out", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp3.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_dynamic_exp3.out", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp4.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_dynamic_exp4.out", "childthread1: SUCCESS, childthread2: SUCCESS, main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp5.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_dynamic_exp5.out", "main: SUCCESS, thread: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp6.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_dynamic_exp6.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp7.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "barrier_dynamic_exp7.out", "CalculationInTask: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/benchmark/barnes.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "benchmark_barnes.out", "ANLinit: SUCCESS,  ComputeForces: SUCCESS, Housekeep: SUCCESS, find_my_bodies: SUCCESS, hackcofm: SUCCESS, maketree: SUCCESS, stepsystem: SUCCESS, find_my_initial_bodies: SUCCESS, SlaveStart: SUCCESS, diagnostics: SUCCESS, initoutput: SUCCESS, initparam: SUCCESS, startrun: SUCCESS, tab_init: SUCCESS, main: SUCCESS, output: SUCCESS")
  }

  def addParahipTests(suite : HipTestSuite) = {
    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/simple.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "simple.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/forkjoin.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "forkjoin.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/cell.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "cell.out", " test: SUCCESS, test1: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/cell4.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "cell4.out", " inc: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/cell-lock-vperm.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "cell_lock_vperm.out", " testCell: SUCCESS, testVar: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/cell-extreme-cases.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "cell_extreme_cases.out", " test: FAIL, test2: FAIL, test3: FAIL, test4: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/ls-bind.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "ls_bind.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/ls-waitlevel2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "ls_waitlevel2.out", " func2: SUCCESS, func3: SUCCESS, func4: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/double-acquire.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "double_acquire.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "no_deadlock1.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "no_deadlock2.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "no_deadlock3.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "deadlock1.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "deadlock2.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "deadlock3.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/disj-no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "disj_no_deadlock1.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/disj-no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "disj_no_deadlock2.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/disj-no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "disj_no_deadlock3.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/disj-deadlock.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "disj_deadlock.out", " func: SUCCESS, main: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/ordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "ordered_locking.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/unordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "unordered_locking.out", " func: FAIL, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/multicast.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "multicast.out", " initialize: SUCCESS, thread: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/oracle.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "oracle.out", " thread: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/owicki-gries.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "owicki_gries.out", " incrementor2: SUCCESS, incrementor1: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/fibonacci.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "fibonacci.out", " seq_fib: SUCCESS, para_fib: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/create_and_acquire.ss", "  --en-para -tp parahip --dis-locklevel --en-thrd-and-conj", OUTPUT_DIR, "create_and_acquire.out", " create_and_acquire: SUCCESS")
  }

  def addVpermTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/alt_threading.ss", "  --ann-vp", OUTPUT_DIR, "vperm_alt_threading.out", " increment: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/fibonacci.ss", "  --ann-vp -tp z3 -perm none --dis-ls dis--locklevel", OUTPUT_DIR, "vperm_fibonacci.out", " seq_fib: SUCCESS, para_fib2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/global-var-norace.ss", "  --ann-vp --dis-pgbv", OUTPUT_DIR, "vperm_global_var_norace.out", " inc: SUCCESS, func: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/global-var-race.ss", "  --ann-vp --dis-pgbv", OUTPUT_DIR, "vperm_global_var_race.out", " inc: SUCCESS, func: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/mergesort.ss", "  --ann-vp", OUTPUT_DIR, "vperm_mergesort.out", " count: SUCCESS, split_func: SUCCESS, merge: SUCCESS, insert: SUCCESS, parallel_merge_sort2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/passive_stack_race.ss", "  --ann-vp", OUTPUT_DIR, "vperm_passive_stack_race.out", " assign: SUCCESS, stack_race: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/stack_race.ss", "  --ann-vp", OUTPUT_DIR, "vperm_stack_race.out", " assign: SUCCESS, stack_race: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/quicksort.ss", "  --ann-vp", OUTPUT_DIR, "vperm_quicksort.out", " partition: SUCCESS, append_bll: SUCCESS, para_qsort2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/task_decompose.ss", "  --ann-vp", OUTPUT_DIR, "vperm_task_decompose.out", " inc: SUCCESS, creator: SUCCESS, joiner: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/threads.ss", "  --ann-vp", OUTPUT_DIR, "vperm_threads.out", " make_tree: SUCCESS, tree_compute_sum_facs: SUCCESS, summator: SUCCESS, start_sum_thread: SUCCESS, join_sum_thread: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/tree_count.ss", "  --ann-vp", OUTPUT_DIR, "vperm_tree_count.out", " parallelCount2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/tree_search.ss", "  --ann-vp -tp mona -perm none", OUTPUT_DIR, "vperm_tree_search.out", " para_search2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/vperm_check.ss", "  --ann-vp", OUTPUT_DIR, "vperm_vperm_check.out", " inc: SUCCESS, incCell: SUCCESS, test1: FAIL, test2: FAIL, test3: FAIL, test4: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm/vperm_simple.ss", "  --ann-vp", OUTPUT_DIR, "vperm_vperm_simple.out", " foo: SUCCESS, f: SUCCESS, foo2: SUCCESS, f2: SUCCESS")
  }

  def addHipTests(suite : HipTestSuite) : Unit = {
    suite addTest (HIP_COMMAND, BASE_DIR + "eps.ss", "  ", OUTPUT_DIR, "eps.out", " get_next: SUCCESS, get_next_next: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "append.ss", "  ", OUTPUT_DIR, "append.out", " append: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "append-tail.ss", "  ", OUTPUT_DIR, "append_tail.out", "append: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "avl-bind.ss", "  ", OUTPUT_DIR, "avl_bind.out", " height: SUCCESS,  rotate_left: SUCCESS,  rotate_right: SUCCESS,  get_max: SUCCESS,  rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "avl.ss", "	 ", OUTPUT_DIR, "avl.out", "  height: SUCCESS, rotate_left: SUCCESS, rotate_right: SUCCESS, get_max: SUCCESS, rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS, insert_inline: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "avl-orig-2.ss", "  ", OUTPUT_DIR, "avl_orig_2.out", "height: SUCCESS, get_max: SUCCESS, insert: SUCCESS, double_left_child: SUCCESS, double_right_child: SUCCESS, rotate_left_child: SUCCESS,  rotate_right_child: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "avl-orig3.ss", " ", OUTPUT_DIR, "avl_orig3.out", " height: SUCCESS, get_max: SUCCESS, insert: SUCCESS, 	double_left_child: SUCCESS, double_right_child: SUCCESS, 	rotate_left_child: SUCCESS, rotate_right_child: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bll.ss", "  ", OUTPUT_DIR, "bll.out", " insert: SUCCESS, delete: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "bubble.ss", "  ", OUTPUT_DIR, "bubble.out", " id2: SUCCESS, id3: SUCCESS, bubble: SUCCESS, bsort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "cll.ss", "  ", OUTPUT_DIR, "cll.out", " test: SUCCESS, insert: SUCCESS, count_rest: SUCCESS, count: SUCCESS, delete: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "complete.ss", " ", OUTPUT_DIR, "complete.out", " maxim: SUCCESS, minim: SUCCESS, height: SUCCESS, min_height: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "dll.ss", " ", OUTPUT_DIR, "dll.out", " insert: SUCCESS, delete: SUCCESS, delete1: SUCCESS, test_del: SUCCESS, test_del2: SUCCESS, test_fold: SUCCESS, append: SUCCESS, append1: SUCCESS, f1: SUCCESS, f2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "heaps.ss", " ", OUTPUT_DIR, "heaps.out", " insert: SUCCESS, deleteoneel: SUCCESS, deleteone: SUCCESS, ripple: SUCCESS, deletemax: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "insertion.ss", " ", OUTPUT_DIR, "insertion.out", " insert: SUCCESS, insertion_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "ll.ss", " ", OUTPUT_DIR, "ll.out", " append: SUCCESS, ret_first: SUCCESS, get_next: SUCCESS, set_next: SUCCESS, set_null: SUCCESS, get_next_next: SUCCESS, insert: SUCCESS, delete: SUCCESS, create_list: SUCCESS, reverse: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "merge.ss", " ", OUTPUT_DIR, "merge.out", " count: SUCCESS, split_func: SUCCESS, merge_sort: SUCCESS, merge: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "perfect.ss", " ", OUTPUT_DIR, "perfect.out", " simple_insert: SUCCESS, create: SUCCESS, maxim: SUCCESS, height: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "qsort.ss", " ", OUTPUT_DIR, "qsort.out", " partition: SUCCESS, append_bll: SUCCESS, qsort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "selection.ss", " ", OUTPUT_DIR, "selection.out", " find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "sll.ss", " ", OUTPUT_DIR, "sll.out", " insert: SUCCESS, insert2: SUCCESS, delete: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "trees.ss", " ", OUTPUT_DIR, "trees.out", " append: SUCCESS, count: SUCCESS, flatten: SUCCESS, insert: SUCCESS, remove_min: SUCCESS, delete: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "rb.ss", " ", OUTPUT_DIR, "rb.out", " rotate_case_3: SUCCESS, case_2: SUCCESS, rotate_case_3r: SUCCESS, case_2r: SUCCESS, is_red: SUCCESS, is_black: SUCCESS, del_6: SUCCESS, del_6r: SUCCESS, del_5: SUCCESS, del_5r: SUCCESS, del_4: SUCCESS, del_4r: SUCCESS, del_3: SUCCESS, del_3r: SUCCESS, del_2: SUCCESS, remove_min: SUCCESS, del: SUCCESS, insert: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "global1.ss", " ", OUTPUT_DIR, "global1.out", " increase: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "global2.ss", " ", OUTPUT_DIR, "global2.out", " increase: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "global3.ss", " ", OUTPUT_DIR, "global3.out", " increase: SUCCESS, increase_n: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "global4.ss", " ", OUTPUT_DIR, "global4.out", " increase_n: SUCCESS, main:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "global5.ss", " ", OUTPUT_DIR, "global5.out", " increase: SUCCESS, decrease: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "global-ll.ss", " ", OUTPUT_DIR, "global_ll.out", " insert_rec: SUCCESS, delete_last_rec: SUCCESS, insert: SUCCESS, delete_last: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "global-mutual-rec.ss", " ", OUTPUT_DIR, "global_mutual_rec.out", " decrease1: SUCCESS, decrease2: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "classic/classic1.ss", " ", OUTPUT_DIR, "classic_classic1.out", " foo1:  SUCCESS,  foo2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "classic/classic1.ss", " --classic", OUTPUT_DIR, "classic_classic1.out", " foo1:  FAIL,  foo2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "classic/classic1a.ss", " ", OUTPUT_DIR, "classic_classic1a.out", " foo1:  SUCCESS,  foo2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "classic/classic1a.ss", " --classic", OUTPUT_DIR, "classic_classic1a.out", " foo1:  SUCCESS,  foo2:  FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "classic/classic2.ss", " ", OUTPUT_DIR, "classic_classic2.out", " foo1:  FAIL,  foo2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "classic/classic2a.ss", " ", OUTPUT_DIR, "classic_classic2a.out", " foo1:  SUCCESS,  foo2:  FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "classic/classic3.ss", " ", OUTPUT_DIR, "classic_classic3.out", " foo1:  SUCCESS,  foo2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "classic/classic3a.ss", " ", OUTPUT_DIR, "classic_classic3a.out", " foo1:  SUCCESS,  foo2:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../../modular_examples/dll-modular.ss", " --overeps", OUTPUT_DIR, ".._.._modular_examples_dll_modular.out", " append:  SUCCESS,  append1:  SUCCESS,  append2:  SUCCESS, delete:  SUCCESS,  delete1:  SUCCESS,  f1:  SUCCESS, f2:  SUCCESS,  insert:  SUCCESS,  test_del:  SUCCESS, test_del2:  SUCCESS,  test_fold:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../../modular_examples/selection-modular.ss", " --overeps --lda", OUTPUT_DIR, ".._.._modular_examples_selection_modular.out", " delete_min:  SUCCESS,  find_min:  SUCCESS,  selection_sort:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../../modular_examples/qsort-modular.ss", " --overeps --lda", OUTPUT_DIR, ".._.._modular_examples_qsort_modular.out", " append_bll:  SUCCESS,  partition:  SUCCESS,  qsort:  SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/vperm_check.ss", "  --ann-vp", OUTPUT_DIR, "vperm_vperm_check.out", " inc: SUCCESS, incCell: SUCCESS, test1: FAIL, test2: FAIL, test3: FAIL, test4: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "vperm/task_decompose.ss", "  --ann-vp", OUTPUT_DIR, "vperm_task_decompose.out", " inc: SUCCESS, creator: SUCCESS, joiner: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/cell.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "parahip_cell.out", " test: SUCCESS, test1: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/cell-extreme-cases.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "parahip_cell_extreme_cases.out", " test: FAIL, test2: FAIL, test3: FAIL, test4: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/ordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "parahip_ordered_locking.out", " func: SUCCESS, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "parahip/unordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-and-conj", OUTPUT_DIR, "parahip_unordered_locking.out", " func: FAIL, main: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/hip-bperm1.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_hip_bperm1.out", "destroyCellFail: FAIL, readCell: SUCCESS, testNewCell: SUCCESS, testNewCell2: SUCCESS, updateCell: SUCCESS, updateCellFail: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-static-consistency.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_static_consistency.out", "main: SUCCESS, main_fail: FAIL, participant: SUCCESS, participant1: SUCCESS, participant_fail: FAIL")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp3.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_dynamic_exp3.out", "main: FAIL, thread1: SUCCESS, thread2: SUCCESS, thread3: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "veribsync/barrier-dynamic-exp4.ss", "  --en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_dynamic_exp4.out", "childthread1: SUCCESS, childthread2: SUCCESS, main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "conchip/mapreduce.ss", "  -tp parahip --classic", OUTPUT_DIR, "conchip_mapreduce.out", "count_helper: SUCCESS, countList: SUCCESS, destroyList: SUCCESS, main: SUCCESS, mapper_helper: SUCCESS, mapper: SUCCESS, reducer1: SUCCESS, reducer2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "conchip/multi-join2.ss", "  -tp parahip  -perm fperm --classic", OUTPUT_DIR, "conchip_multi_join2.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "conchip/latch-exp2.ss", "  -tp parahip --classic", OUTPUT_DIR, "conchip_latch_exp2.out", "main: FAIL, thread1: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "conchip/deadpool.ss", "  -tp parahip -perm fperm --classic", OUTPUT_DIR, "conchip_deadpool.out", "destroyDeadPool: SUCCESS, forkHelper: SUCCESS, forkThreads: SUCCESS, joinHelper: SUCCESS, joinThreads: SUCCESS, main: SUCCESS, thread: SUCCESS")
  }

  def addHipBarrTests(suite : HipTestSuite) = {
    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/video_ex1_th3.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_video_ex1_th3.out", " th1_loop: SUCCESS, th1: SUCCESS, th2_loop: SUCCESS, th2: SUCCESS, th3_loop: SUCCESS, th3: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/SIMD_ex1_v2_th3.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_SIMD_ex1_v2_th3.out", " controll: SUCCESS, control: SUCCESS, thl1: SUCCESS, th1: SUCCESS, thl2: SUCCESS, th2: SUCCESS, thl3: SUCCESS, th3: SUCCESS, thl4: SUCCESS, th4: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/SIMD_ex1_th1.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_SIMD_ex1_th1.out", " controll: SUCCESS, control: SUCCESS, thl1: SUCCESS, th1: SUCCESS, thl2: SUCCESS, th2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/PIPE_ex1_th5.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_PIPE_ex1_th5.out", " controll: SUCCESS, thl5: SUCCESS, thl1: SUCCESS, thl2: SUCCESS, thl3: SUCCESS, thl4: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/MIXED_ex1_th4.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_MIXED_ex1_th4.out", " ctl12: SUCCESS, ctl11: SUCCESS, starterc: SUCCESS, thl12: SUCCESS, thl22: SUCCESS, thl21: SUCCESS, thl11: SUCCESS, startert1: SUCCESS, startert2: SUCCESS, thl32: SUCCESS, thl31: SUCCESS, startert3: SUCCESS, thl42: SUCCESS, thl41: SUCCESS, startert4: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/MISD_ex2_th5.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_MISD_ex2_th5.out", " controll: SUCCESS, control: SUCCESS, thl: SUCCESS, th: SUCCESS, thl2: SUCCESS, th2: SUCCESS, thl3: SUCCESS, th3: SUCCESS, thl4: SUCCESS, th4: SUCCESS, thl5: SUCCESS, th5: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/MISD_ex1_th5.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_MISD_ex1_th5.out", " controll: SUCCESS, control: SUCCESS, thl: SUCCESS, th: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/barrier-weak.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_barrier_weak.out", " th1_loop: SUCCESS, th1: SUCCESS, th2_loop: SUCCESS, th2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/barrier-strong.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_barrier_strong.out", " th1_loop: SUCCESS, th1: SUCCESS, th2_loop: SUCCESS, th2: SUCCESS")

    suite addTest (HIP_COMMAND, BASE_DIR + "../tree_shares/thesis/barrier-paper.ss", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_thesis_barrier_paper.out", " th1_loop: SUCCESS, th1: SUCCESS, th2_loop: SUCCESS, th2: SUCCESS")
  }
}
