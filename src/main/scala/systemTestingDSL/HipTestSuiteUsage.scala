package systemTestingDSL

import systemTestingDSL.testSuite.HipTestSuite

object HipTestSuiteUsage {
  def run(): Unit = {
    val suite = new HipTestSuite()
    //    addInfinityTests(suite)
    //    addArrayTests(suite)
    //    addListTests(suite)
    //    addTermTests(suite)
    //    addListTests(suite)
    //    addImmTests(suite)
    //    addThreadTests(suite)
    addHipBagaTests(suite)

    // Debug this one
    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm-field/sll.ss", "-tp oc --field-ann --etcsu1 ", "results", "sll.out", "delete: SUCCESS, get_tail: SUCCESS, insert: SUCCESS, insert2: SUCCESS")
    suite.runAllTests
    suite generateTestStatistics
  }

  def addInfinityTests(suite: HipTestSuite): Unit = {
    // Infinity

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/inflist.ss", "--dsd --en-inf", "results", "inflist.out", "remove: SUCCESS, append: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/infll_take.ss", "--dsd --en-inf", "results", "infll_take.out", "take: SUCCESS")

    //    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/inftree.ss", "--dsd --en-inf", "results", "inftree.out", "count: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/stream.ss", "--dsd --en-inf", "results", "stream.out", "zip: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/bst-inf.ss", "--dsd --en-inf", "results", "bst_inf.out", "delete: SUCCESS, remove_min: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/inf-selsort.ss", "--dsd --en-disj-compute --en-inf", "results", "inf_selsort.out", "find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/inf-ins.ss", "--dsd --en-inf", "results", "inf_ins.out", "insert: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/inf-sel.ss", "--dsd --en-inf", "results", "inf_sel.out", "find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/bubble-inf.ss", "--dsd --en-inf", "results", "bubble_inf.out", "id2: SUCCESS, id3: SUCCESS, bubble: SUCCESS, bsort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/heaps-inf.ss", "--en-inf", "results", "heaps_inf.out", "insert: SUCCESS, deleteoneel: SUCCESS, deleteone: SUCCESS, deletemax: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/infinity/merge-inf.ss", "--dsd --en-inf --en-disj-compute", "results", "merge_inf.out", "merge: SUCCESS")

  }

  def addArrayTests(suite: HipTestSuite): Unit = {

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_at.java", "", "results", "arr_at.out", "main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_binarysearch.java", "", "results", "arr_binarysearch.out", "binary_search: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_search_decrease_less_than_two.java", "", "results", "arr_search_decrease_less_than_two.out", "searchzero: FAIL")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_bubblesort.java", "", "results", "arr_bubblesort.out", "bubblesort: SUCCESS, bubble: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_bubblesort_perm.java", "", "results", "arr_bubblesort_perm.out", "bubblesort: SUCCESS, bubble: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_double.java", "", "results", "arr_double.out", "doublearr: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_extract_nonzeros.java", "", "results", "arr_extract_nonzeros.out", "copy_nonzeros: SUCCESS, count_nonzeros: SUCCESS, extract_nonzeros: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_init.java", "", "results", "arr_init.out", "zinit: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_insertsort.java", "", "results", "arr_insertsort.out", "insertelm: SUCCESS, insertion_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_insertsort_perm.java", "", "results", "arr_insertsort_perm.out", "insertelm: SUCCESS, insertion_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_invert.java", "", "results", "arr_invert.out", "Invert: SUCCESS, InvertHelper: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_max.java", "", "results", "arr_max.out", "max_value_of_array: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_mergesort.java", "", "results", "arr_mergesort.out", "merge_sorted_arrays: SUCCESS, copy_array: SUCCESS, merge_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_new_exp.java", "", "results", "arr_new_exp.out", "main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_nqueens.java", "", "results", "arr_nqueens.out", "nQueens: SUCCESS, nQueensHelper: SUCCESS, nQueensHelperHelper: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_qsort.java", "", "results", "arr_qsort.out", "arraypart: SUCCESS, qsort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_rev.java", "", "results", "arr_rev.out", "arrayrev: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_selectionsort.java", "", "results", "arr_selectionsort.out", "array_index_of_max: SUCCESS, selection_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_selectionsort_perm.java", "", "results", "arr_selectionsort_perm.out", "array_index_of_max: SUCCESS, selection_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_sparse.java", "--imm", "results", "arr_sparse.out", "create: SUCCESS, get: SUCCESS, setsa: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/array/arr_sum.java", "", "results", "arr_sum.out", "sigmaright: SUCCESS, sigmaleft: SUCCESS")
  }

  def addListTests(suite: HipTestSuite): Unit = {
    // Lists

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/demo.ss", " ", "results", "demo.out", ":  reverse,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/demo2.ss", " ", "results", "demo2.out", " app_rev:  SUCCESS,  reverse:  SUCCESS,  append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/err-coq.ss", " ", "results", "err_coq.out", " ret_first2:  SUCCESS,  ret_first:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll.ss", " ", "results", "ll.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll_bak.ss", " ", "results", "ll_bak.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll_bak2.ss", " ", "results", "ll_bak2.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll_bak3.ss", " ", "results", "ll_bak3.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll_nolists.ss", " ", "results", "ll_nolists.out", " reverse:  SUCCESS,  create_list:  SUCCESS,  delete_val:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS,  get_next_next:  SUCCESS,  set_null:  SUCCESS,  set_next:  SUCCESS,  get_next:  SUCCESS,  ret_first:  SUCCESS,  append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll_test1.ss", " ", "results", "ll_test1.out", " reverse:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll_test2.ss", " ", "results", "ll_test2.out", " delete:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll_test4.ss", " ", "results", "ll_test4.out", " test:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/ll_test5.ss", " ", "results", "ll_test5.out", " delete_val:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/lrev-bug.ss", " ", "results", "lrev_bug.out", " lrev:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/lrev.ss", " ", "results", "lrev.out", " lrev:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/merge.ss", " ", "results", "merge.out", " merge:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/merge1.ss", " ", "results", "merge1.out", " merge:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/merge2.ss", " ", "results", "merge2.out", " merge:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/merge3.ss", " ", "results", "merge3.out", " merge:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/mk_zero.ss", " ", "results", "mk_zero.out", " mk_zero:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/lists/perm.ss", " ", "results", "perm.out", " append:  SUCCESS")
  }
  def addTermTests(suite: HipTestSuite): Unit = {

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/e1.ss", " ", "results", "e1.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex1.ss", " ", "results", "ex1.out", " length:  SUCCESS,  app2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex10.ss", " ", "results", "ex10.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex11.ss", " ", "results", "ex11.out", " bsearch:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex15.ss", " ", "results", "ex15.out", " loop:  SUCCESS,  f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex16.ss", " ", "results", "ex16.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex2.ss", " ", "results", "ex2.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex3.ss", " ", "results", "ex3.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex4.ss", " ", "results", "ex4.out", " inc_loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex5.ss", " ", "results", "ex5.out", " foo:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex6.ss", " ", "results", "ex6.out", " Ack:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex7.ss", " ", "results", "ex7.out", " loop_aux1:  SUCCESS,  loop_aux:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex8.ss", " ", "results", "ex8.out", " loop2:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/ex9.ss", " ", "results", "ex9.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/mutual.ss", " ", "results", "mutual.out", " g:  SUCCESS,  f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/cav08-1.ss", " ", "results", "benchs_lit_cav08_1.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/cav08-2.ss", " ", "results", "benchs_lit_cav08_2.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/cav08-3.ss", " ", "results", "benchs_lit_cav08_3.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/cav08-4.ss", " ", "results", "benchs_lit_cav08_4.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/cav08-5.ss", " ", "results", "benchs_lit_cav08_5.out", " loop:  SUCCESS,  f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/cav08-6.ss", " ", "results", "benchs_lit_cav08_6.out", " gcd:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/dijkstra76-1.ss", " ", "results", "benchs_lit_dijkstra76_1.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/dijkstra76-2.ss", " ", "results", "benchs_lit_dijkstra76_2.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/dijkstra76-3.ss", " ", "results", "benchs_lit_dijkstra76_3.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/pldi06-1.ss", " ", "results", "benchs_lit_pldi06_1.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/pldi06-2.ss", " ", "results", "benchs_lit_pldi06_2.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/pldi06-3.ss", " ", "results", "benchs_lit_pldi06_3.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/pldi06-4.ss", " ", "results", "benchs_lit_pldi06_4.out", " main:  SUCCESS,  loop:  SUCCESS,  loop_aux:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/pldi06-5.ss", " ", "results", "benchs_lit_pldi06_5.out", " Ack:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/popl07-1.ss", " ", "results", "benchs_lit_popl07_1.out", " loop_1:  SUCCESS,  loop_2:  SUCCESS,  loop_3:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/popl07-2.ss", " ", "results", "benchs_lit_popl07_2.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/sas05.ss", " ", "results", "benchs_lit_sas05.out", " loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/sas10-1.ss", " ", "results", "benchs_lit_sas10_1.out", " f:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/sas10-2.ss", " ", "results", "benchs_lit_sas10_2.out", " foo:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/sas10-2a.ss", " ", "results", "benchs_lit_sas10_2a.out", " foo:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/sas10-3.ss", " ", "results", "benchs_lit_sas10_3.out", " main:  SUCCESS,  foo:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/vcc-1.ss", " ", "results", "benchs_lit_vcc_1.out", " f:  SUCCESS,  g:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/vmcai05-1a.ss", " ", "results", "benchs_lit_vmcai05_1a.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/lit/vmcai05-1b.ss", " -tp redlog", "results", "benchs_lit_vmcai05_1b.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/AlternatingIncr.ss", " ", "results", "benchs_key_AlternatingIncr.out", " increase:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/AlternDiv-invalid-1.ss", " -tp redlog", "results", "benchs_key_AlternDiv_invalid_1.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/AlternDiv.ss", " -tp redlog", "results", "benchs_key_AlternDiv.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/AlternDivWidening.ss", " -tp redlog", "results", "benchs_key_AlternDivWidening.out", " loop:  SUCCESS,  loop_aux:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/AlternDivWide.ss", " ", "results", "benchs_key_AlternDivWide.out", " loop:  SUCCESS,  loop_aux:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/AlternKonv.ss", " ", "results", "benchs_key_AlternKonv.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Collatz.ss", " ", "results", "benchs_key_Collatz.out", " collatz:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/ComplInterv2.ss", " ", "results", "benchs_key_ComplInterv2.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/ComplInterv3.ss", " ", "results", "benchs_key_ComplInterv3.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/ComplInterv.ss", " -tp z3", "results", "benchs_key_ComplInterv.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/ComplxStruc-may.ss", " ", "results", "benchs_key_ComplxStruc_may.out", " complxStruc:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/ComplxStruc2.ss", " ", "results", "benchs_key_ComplxStruc2.out", " loop:  SUCCESS,  complxStruc:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/ConvLower.ss", " ", "results", "benchs_key_ConvLower.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Cousot.ss", " ", "results", "benchs_key_Cousot.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/DoubleNeg.ss", " -tp redlog", "results", "benchs_key_DoubleNeg.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Even.ss", " ", "results", "benchs_key_Even.out", " even:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex01.ss", " ", "results", "benchs_key_Ex01.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex02.ss", " ", "results", "benchs_key_Ex02.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex03.ss", " ", "results", "benchs_key_Ex03.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex04.ss", " ", "results", "benchs_key_Ex04.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex05.ss", " ", "results", "benchs_key_Ex05.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex06.ss", " ", "results", "benchs_key_Ex06.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex07.ss", " ", "results", "benchs_key_Ex07.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex08.ss", " ", "results", "benchs_key_Ex08.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Ex09.ss", " ", "results", "benchs_key_Ex09.out", " half:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Fibonacci.ss", " ", "results", "benchs_key_Fibonacci.out", " fib:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Flip2.ss", " ", "results", "benchs_key_Flip2.out", " flip:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Flip3.ss", " ", "results", "benchs_key_Flip3.out", " flip:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Flip.ss", " ", "results", "benchs_key_Flip.out", " flip:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Gauss.ss", " ", "results", "benchs_key_Gauss.out", " sum:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Gcd-may.ss", " ", "results", "benchs_key_Gcd_may.out", " gcd:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Lcm.ss", " ", "results", "benchs_key_Lcm.out", " lcm:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Marbie1.ss", " ", "results", "benchs_key_Marbie1.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Marbie2.ss", " ", "results", "benchs_key_Marbie2.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Middle.ss", " ", "results", "benchs_key_Middle.out", " middle:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/MirrorIntervSim.ss", " ", "results", "benchs_key_MirrorIntervSim.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/MirrorInterv.ss", " ", "results", "benchs_key_MirrorInterv.out", " mirrorInterv:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/ModuloLower.ss", " ", "results", "benchs_key_ModuloLower.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/ModuloUp.ss", " -tp redlog", "results", "benchs_key_ModuloUp.out", " up:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Narrowing.ss", " ", "results", "benchs_key_Narrowing.out", " narrowing:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/NarrowKonv.ss", " ", "results", "benchs_key_NarrowKonv.out", " narrowKonv:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/NegPos.ss", " -tp redlog", "results", "benchs_key_NegPos.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Plait-may.ss", " ", "results", "benchs_key_Plait_may.out", " plait:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/Sunset.ss", " ", "results", "benchs_key_Sunset.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/TrueDiv.ss", " ", "results", "benchs_key_TrueDiv.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/TwoFloatInterv.ss", " ", "results", "benchs_key_TwoFloatInterv.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/UpAndDownIneq.ss", " ", "results", "benchs_key_UpAndDownIneq.out", " upAndDown:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/UpAndDown.ss", " ", "results", "benchs_key_UpAndDown.out", " upAndDown:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileBreak.ss", " ", "results", "benchs_key_WhileBreak.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileDecr.ss", " ", "results", "benchs_key_WhileDecr.out", " decrease:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileIncrPart.ss", " ", "results", "benchs_key_WhileIncrPart.out", " increase:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileIncr.ss", " ", "results", "benchs_key_WhileIncr.out", " increase:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileNestedOffset.ss", " ", "results", "benchs_key_WhileNestedOffset.out", " increase:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileNested.ss", " ", "results", "benchs_key_WhileNested.out", " increase:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhilePart.ss", " ", "results", "benchs_key_WhilePart.out", " increase:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileSingle.ss", " ", "results", "benchs_key_WhileSingle.out", " increase:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileSum.ss", " ", "results", "benchs_key_WhileSum.out", " increase:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/key/WhileTrue.ss", " ", "results", "benchs_key_WhileTrue.out", " endless:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/DivMinus2.ss", " ", "results", "benchs_aprove_Aprove_09_DivMinus2.out", " main:  SUCCESS,  div:  SUCCESS,  minus:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/DivMinus.ss", " ", "results", "benchs_aprove_Aprove_09_DivMinus.out", " main:  SUCCESS,  div:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/DivWithoutMinus.ss", " ", "results", "benchs_aprove_Aprove_09_DivWithoutMinus.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/Duplicate.ss", " ", "results", "benchs_aprove_Aprove_09_Duplicate.out", " main:  SUCCESS,  round:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/GCD2.ss", " -tp redlog", "results", "benchs_aprove_Aprove_09_GCD2.out", " main:  SUCCESS,  gcd:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/GCD3.ss", " ", "results", "benchs_aprove_Aprove_09_GCD3.out", " main:  SUCCESS,  gcd:  SUCCESS,  mod:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/GCD4.ss", " ", "results", "benchs_aprove_Aprove_09_GCD4.out", " main:  SUCCESS,  gcd:  SUCCESS,  mod:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/GCD5.ss", " -tp redlog", "results", "benchs_aprove_Aprove_09_GCD5.out", " main:  SUCCESS,  gcd:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/GCD.ss", " -tp redlog", "results", "benchs_aprove_Aprove_09_GCD.out", " main:  SUCCESS,  gcd:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/LogAG.ss", " ", "results", "benchs_aprove_Aprove_09_LogAG.out", " main:  SUCCESS,  half:  SUCCESS,  log:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/LogBuiltIn.ss", " ", "results", "benchs_aprove_Aprove_09_LogBuiltIn.out", " main:  SUCCESS,  log:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/LogIterative.ss", " -tp redlog", "results", "benchs_aprove_Aprove_09_LogIterative.out", " main:  SUCCESS,  log:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/LogMult.ss", " -tp redlog", "results", "benchs_aprove_Aprove_09_LogMult.out", " main:  SUCCESS,  log:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/Log.ss", " ", "results", "benchs_aprove_Aprove_09_Log.out", " main:  SUCCESS,  half:  SUCCESS,  log:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/McCarthyIterative-may.ss", " ", "results", "benchs_aprove_Aprove_09_McCarthyIterative_may.out", " mcCarthy:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/McCarthyRec.ss", " ", "results", "benchs_aprove_Aprove_09_McCarthyRec.out", " mcCarthy:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/MinusBuiltIn.ss", " ", "results", "benchs_aprove_Aprove_09_MinusBuiltIn.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/MinusMin.ss", " ", "results", "benchs_aprove_Aprove_09_MinusMin.out", " main:  SUCCESS,  mn:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/MinusUserDefined.ss", " ", "results", "benchs_aprove_Aprove_09_MinusUserDefined.out", " main:  SUCCESS,  gt:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/Mod.ss", " ", "results", "benchs_aprove_Aprove_09_Mod.out", " main:  SUCCESS,  mod:  SUCCESS,  minus:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/PlusSwap.ss", " ", "results", "benchs_aprove_Aprove_09_PlusSwap.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Aprove_09/Round3.ss", " ", "results", "benchs_aprove_Aprove_09_Round3.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/AProVE_10/AG313.ss", " ", "results", "benchs_aprove_AProVE_10_AG313.out", " main:  SUCCESS,  quot:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/AProVE_11_iterative/RetValRec.ss", " ", "results", "benchs_aprove_AProVE_11_iterative_RetValRec.out", " main:  SUCCESS,  ret:  SUCCESS,  test:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/AProVE_11_iterative/RetVal.ss", " ", "results", "benchs_aprove_AProVE_11_iterative_RetVal.out", " main:  SUCCESS,  ret:  SUCCESS,  test:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/AProVE11NO/LoopingNonTerm.ss", " ", "results", "benchs_aprove_AProVE11NO_LoopingNonTerm.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/AProVE11NO/NonPeriodicNonTerm2.ss", " ", "results", "benchs_aprove_AProVE11NO_NonPeriodicNonTerm2.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/Avg.ss", " ", "results", "benchs_aprove_BOG_RTA_11_Avg.out", " avg:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/EqUserDefRec.ss", " ", "results", "benchs_aprove_BOG_RTA_11_EqUserDefRec.out", " main:  SUCCESS,  eq:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/Fibonacci.ss", " ", "results", "benchs_aprove_BOG_RTA_11_Fibonacci.out", " main:  SUCCESS,  fib:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/LeUserDefRec.ss", " ", "results", "benchs_aprove_BOG_RTA_11_LeUserDefRec.out", " main:  SUCCESS,  le:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/LogRecursive.ss", " -tp redlog", "results", "benchs_aprove_BOG_RTA_11_LogRecursive.out", " main:  SUCCESS,  log:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/Nest.ss", " ", "results", "benchs_aprove_BOG_RTA_11_Nest.out", " main:  SUCCESS,  nest:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/TerminatiorRec01.ss", " ", "results", "benchs_aprove_BOG_RTA_11_TerminatiorRec01.out", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/TerminatiorRec02.ss", " -tp redlog", "results", "benchs_aprove_BOG_RTA_11_TerminatiorRec02.out", " fact:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/TerminatiorRec03.ss", " ", "results", "benchs_aprove_BOG_RTA_11_TerminatiorRec03.out", " f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/TerminatiorRec04-modified.ss", " ", "results", "benchs_aprove_BOG_RTA_11_TerminatiorRec04_modified.out", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/TerminatiorRec04.ss", " ", "results", "benchs_aprove_BOG_RTA_11_TerminatiorRec04.out", " main:  SUCCESS,  f:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/TimesPlusUserDef.ss", " ", "results", "benchs_aprove_BOG_RTA_11_TimesPlusUserDef.out", " main:  SUCCESS,  times:  SUCCESS,  plus:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/BOG_RTA_11/TwoWay.ss", " -tp redlog", "results", "benchs_aprove_BOG_RTA_11_TwoWay.out", " twoWay:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Break.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Break.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Continue1.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Continue1.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Continue.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Continue.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/costa09-example_1.ss", " ", "results", "benchs_aprove_Costa_Julia_09_costa09_example_1.out", " incr:  SUCCESS,  add:  SUCCESS,  incr2:  SUCCESS,  add2:  SUCCESS,  incr3:  SUCCESS,  add3:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/costa09-example_2.ss", " -tp redlog", "results", "benchs_aprove_Costa_Julia_09_costa09_example_2.out", " main:  SUCCESS,  divBy:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/costa09-example_3.ss", " ", "results", "benchs_aprove_Costa_Julia_09_costa09_example_3.out", " main:  SUCCESS,  m:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc1-exc.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc1_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc2-exc.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc2_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc3-exc.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc3_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc4-exc.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc4_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc5-exc.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc5_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc-exc.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc_exc.out", " main:  SUCCESS,  rec_f:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc1-no.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc1_no.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc2-no.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc2_no.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc3-no.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc3_no.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc4-no.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc4_no.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc5-no.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc5_no.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Exc-no.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Exc_no.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Loop1.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Loop1.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Nested.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Nested.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/Sequence.ss", " ", "results", "benchs_aprove_Costa_Julia_09_Sequence.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09/TestJulia4.ss", " -tp redlog", "results", "benchs_aprove_Costa_Julia_09_TestJulia4.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Ackermann.ss", " ", "results", "benchs_aprove_Costa_Julia_09_recursive_Ackermann.out", " main:  SUCCESS,  ack:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Double-1.ss", " -tp redlog", "results", "benchs_aprove_Costa_Julia_09_recursive_Double_1.out", " test:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Double2-1.ss", " ", "results", "benchs_aprove_Costa_Julia_09_recursive_Double2_1.out", " main:  SUCCESS,  test:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Double2.ss", " ", "results", "benchs_aprove_Costa_Julia_09_recursive_Double2.out", " main:  SUCCESS,  test:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Double3-1.ss", " ", "results", "benchs_aprove_Costa_Julia_09_recursive_Double3_1.out", " main:  SUCCESS,  test:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Double3.ss", " ", "results", "benchs_aprove_Costa_Julia_09_recursive_Double3.out", " main:  SUCCESS,  test:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Double.ss", " -tp redlog", "results", "benchs_aprove_Costa_Julia_09_recursive_Double.out", " main:  SUCCESS,  test:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Factorial.ss", " -tp redlog", "results", "benchs_aprove_Costa_Julia_09_recursive_Factorial.out", " main:  SUCCESS,  fact:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/FactSumList.ss", " -tp redlog", "results", "benchs_aprove_Costa_Julia_09_recursive_FactSumList.out", " doSum:  SUCCESS,  fact:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/FactSum.ss", " -tp redlog", "results", "benchs_aprove_Costa_Julia_09_recursive_FactSum.out", " doSum:  SUCCESS,  fact:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Costa_Julia_09-recursive/Hanoi.ss", " ", "results", "benchs_aprove_Costa_Julia_09_recursive_Hanoi.out", " main:  SUCCESS,  sol:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Iterative/NonPeriodic.ss", " ", "results", "benchs_aprove_Julia_10_Iterative_NonPeriodic.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Iterative/Test11.ss", " -tp redlog", "results", "benchs_aprove_Julia_10_Iterative_Test11.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Iterative/Test2.ss", " ", "results", "benchs_aprove_Julia_10_Iterative_Test2.out", " main:  SUCCESS,  iter:  SUCCESS,  add:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/AckR.ss", " ", "results", "benchs_aprove_Julia_10_Recursive_AckR.out", " main:  SUCCESS,  ack:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/FibSLR.ss", " -tp redlog", "results", "benchs_aprove_Julia_10_Recursive_FibSLR.out", " main:  SUCCESS,  fib:  SUCCESS,  doSum:  SUCCESS,  create:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/HanR.ss", " ", "results", "benchs_aprove_Julia_10_Recursive_HanR.out", " main:  SUCCESS,  sol:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/Power.ss", " -tp redlog", "results", "benchs_aprove_Julia_10_Recursive_Power.out", " power:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/EvenOdd.ss", " ", "results", "benchs_aprove_Julia_10_Recursive_EvenOdd.out", " even:  SUCCESS,  odd:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/Recursions.ss", " ", "results", "benchs_aprove_Julia_10_Recursive_Recursions.out", " main:  SUCCESS,  rec0:  SUCCESS,  rec1:  SUCCESS, rec2:  SUCCESS,  rec3:  SUCCESS,  rec4:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/Test10.ss", " ", "results", "benchs_aprove_Julia_10_Recursive_Test10.out", " main:  SUCCESS,  rec:  SUCCESS,  test:  SUCCESS,  descend:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/Test12.ss", " -tp redlog", "results", "benchs_aprove_Julia_10_Recursive_Test12.out", " main:  SUCCESS,  rec:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_10_Recursive/Test1.ss", " ", "results", "benchs_aprove_Julia_10_Recursive_Test1.out", " main:  SUCCESS,  rec:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/ChooseLife.ss", " ", "results", "benchs_aprove_Julia_11_iterative_ChooseLife.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/Choose.ss", " ", "results", "benchs_aprove_Julia_11_iterative_Choose.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/Continue.ss", " ", "results", "benchs_aprove_Julia_11_iterative_Continue.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/Loop.ss", " -tp redlog", "results", "benchs_aprove_Julia_11_iterative_Loop.out", " main:  SUCCESS,  test:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_00.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_00.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_01.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_01.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_02.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_02.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_03.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_03.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_04.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_04.out", " main:  SUCCESS,  for_1:  SUCCESS,  for_2:  SUCCESS,  for_3:  SUCCESS,  for_4:  SUCCESS,  for_5:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_05.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_05.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_06.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_06.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_10.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_10.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_11.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_11.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_12.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_12.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_20.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_20.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_21.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_21.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_22.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_22.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_23.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_23.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/NO_24.ss", " ", "results", "benchs_aprove_Julia_11_iterative_NO_24.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/Parts.ss", " ", "results", "benchs_aprove_Julia_11_iterative_Parts.out", " parts:  SUCCESS,  main:  SUCCESS,  for_1:  SUCCESS,  loop_1:  SUCCESS,  for_2:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/Julia_11_iterative/Swingers.ss", " ", "results", "benchs_aprove_Julia_11_iterative_Swingers.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaA10.ss", " ", "results", "benchs_aprove_pasta_PastaA10.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaA1.ss", " ", "results", "benchs_aprove_pasta_PastaA1.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaA4.ss", " ", "results", "benchs_aprove_pasta_PastaA4.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaA5.ss", " ", "results", "benchs_aprove_pasta_PastaA5.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaA6.ss", " ", "results", "benchs_aprove_pasta_PastaA6.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaA7.ss", " ", "results", "benchs_aprove_pasta_PastaA7.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaA8.ss", " ", "results", "benchs_aprove_pasta_PastaA8.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaA9.ss", " ", "results", "benchs_aprove_pasta_PastaA9.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB10.ss", " ", "results", "benchs_aprove_pasta_PastaB10.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB11.ss", " ", "results", "benchs_aprove_pasta_PastaB11.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB12.ss", " ", "results", "benchs_aprove_pasta_PastaB12.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB13.ss", " ", "results", "benchs_aprove_pasta_PastaB13.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB14.ss", " ", "results", "benchs_aprove_pasta_PastaB14.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB15.ss", " ", "results", "benchs_aprove_pasta_PastaB15.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB16-loop.ss", " ", "results", "benchs_aprove_pasta_PastaB16_loop.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB16.ss", " ", "results", "benchs_aprove_pasta_PastaB16.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB17.ss", " ", "results", "benchs_aprove_pasta_PastaB17.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB18.ss", " ", "results", "benchs_aprove_pasta_PastaB18.out", " main:  SUCCESS,  loop:  SUCCESS,  decrease:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB1.ss", " ", "results", "benchs_aprove_pasta_PastaB1.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB2.ss", " ", "results", "benchs_aprove_pasta_PastaB2.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB3.ss", " ", "results", "benchs_aprove_pasta_PastaB3.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB4.ss", " ", "results", "benchs_aprove_pasta_PastaB4.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB4-loop.ss", " ", "results", "benchs_aprove_pasta_PastaB4_loop.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB5.ss", " ", "results", "benchs_aprove_pasta_PastaB5.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB6.ss", " ", "results", "benchs_aprove_pasta_PastaB6.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB7.ss", " ", "results", "benchs_aprove_pasta_PastaB7.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaB8.ss", " ", "results", "benchs_aprove_pasta_PastaB8.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC10-while.ss", " ", "results", "benchs_aprove_pasta_PastaC10_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC11.ss", " ", "results", "benchs_aprove_pasta_PastaC11.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC11-while.ss", " ", "results", "benchs_aprove_pasta_PastaC11_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC1.ss", " ", "results", "benchs_aprove_pasta_PastaC1.out", " main:  SUCCESS,  loop_1:  SUCCESS,  loop_2:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC1-while.ss", " ", "results", "benchs_aprove_pasta_PastaC1_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC2-while.ss", " ", "results", "benchs_aprove_pasta_PastaC2_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC3.ss", " ", "results", "benchs_aprove_pasta_PastaC3.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC3-while.ss", " ", "results", "benchs_aprove_pasta_PastaC3_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC4-while.ss", " ", "results", "benchs_aprove_pasta_PastaC4_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC5-while.ss", " ", "results", "benchs_aprove_pasta_PastaC5_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC7-simpl-1.ss", " ", "results", "benchs_aprove_pasta_PastaC7_simpl_1.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC7-simpl-2.ss", " ", "results", "benchs_aprove_pasta_PastaC7_simpl_2.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC7-simpl.ss", " ", "results", "benchs_aprove_pasta_PastaC7_simpl.out", " loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC7.ss", " ", "results", "benchs_aprove_pasta_PastaC7.out", " main:  SUCCESS,  loop:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC7-while.ss", " ", "results", "benchs_aprove_pasta_PastaC7_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC8-while.ss", " ", "results", "benchs_aprove_pasta_PastaC8_while.out", " main:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/term/benchs/aprove/pasta/PastaC9-while.ss", " ", "results", "benchs_aprove_pasta_PastaC9_while.out", " main:  SUCCESS")

  }
  def addImmTests(suite: HipTestSuite): Unit = {

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/bigint.ss", "   --imm -tp redlog", "results", "bigint.out", " clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/bigint_imm.ss", "   --imm -tp redlog", "results", "bigint_imm.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, test:  SUCCESS,  add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult2:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/bigint_imm-star.ss", "   --imm -tp redlog", "results", "bigint_imm_star.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/bigint-tight.ss", "   --imm -tp redlog", "results", "bigint_tight.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/bigint-tight-imm.ss", "   --imm -tp redlog", "results", "bigint_tight_imm.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, test:  SUCCESS,  add_c:  SUCCESS, add:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/bigint-tight-imm-star.ss", "   --imm -tp redlog", "results", "bigint_tight_imm_star.out", "clone:  SUCCESS, int_value:  SUCCESS, bigint_of:  SUCCESS, add_one_digit:  SUCCESS, add_c:  SUCCESS, add:  SUCCESS, sub_one_digit:  SUCCESS, sub_c:  SUCCESS, sub:  SUCCESS, mult_c:  SUCCESS, shift_left:  SUCCESS, mult:  SUCCESS, is_zero:  SUCCESS, is_equal:  SUCCESS, compare2:  SUCCESS, compare_int:  SUCCESS, div_with_remainder:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/append_imm.ss", "   --imm ", "results", "append_imm.out", " append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/kara.ss", "   --imm -tp redlog", "results", "kara.out", " karatsuba_mult: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/kara-imm-star.ss", "   --imm -tp redlog ", "results", "kara_imm_star.out", " karatsuba_mult: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/kara-imm-conj.ss", "  --imm -tp redlog", "results", "kara_imm_conj.out", " karatsuba_mult: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm/ll_imm.ss", "   --imm ", "results", "ll_imm.out", " length:  SUCCESS, append:  SUCCESS, sumN:  SUCCESS, set_next:  SUCCESS, get_next_next:  SUCCESS, get_next:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm-field/imspd.ss", "-tp oc --field-ann --etcsu1 ", "results", "imspd.out", "check_pass: SUCCESS, login: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm-field/getset.ss", "-tp oc --field-ann --etcsu1 ", "results", "getset.out", "sset: SUCCESS, get: SUCCESS, setA: SUCCESS, getA: SUCCESS, non_negative: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm-field/bigint.ss", "-tp redlog --field-ann --etcsu1 ", "results", "bigint.out", "clone: SUCCESS, add_one_digit: SUCCESS, add_c: SUCCESS, add: SUCCESS, div_with_remainder: SUCCESS, bigint_of: SUCCESS, compare_int: SUCCESS, is_zero: SUCCESS, compare2: SUCCESS, int_value: SUCCESS, mult_c: SUCCESS, shift_left: SUCCESS, mult: SUCCESS, sub_one_digit: SUCCESS, sub_c: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm-field/insertion_simple.ss", "-tp oc --field-ann --etcsu1 ", "results", "insertion_simple.out", "insert: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/imm-field/schorr-waite-list.ss", "-tp om --field-ann --etcsu1 ", "results", "schorr_waite_list.out", "lscan: SUCCESS")
  }

  def addThreadTests(suite: HipTestSuite): Unit = {

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/motiv-example.ss", "  --en-para --en-thrd-resource -tp redlog", "results", "threads_motiv_example.out", " main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/motiv-example2.ss", "  --en-para --en-thrd-resource -tp redlog", "results", "threads_motiv_example2.out", " main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/no-deadlock-nonlexical2.ss", "  --en-para --en-thrd-resource -tp parahip --en-lsmu-infer", "results", "threads_no_deadlock_nonlexical2.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/no-deadlock-nonlexical.ss", "  --en-para --en-thrd-resource -tp parahip --en-lsmu-infer", "results", "threads_no_deadlock_nonlexical.out", "main: SUCCESS, thread1: SUCCESS, thread2: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/forkjoin.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_forkjoin.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/cell4.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_cell4.out", " inc: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/ls-bind.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_ls_bind.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_no_deadlock1.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_no_deadlock2.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_no_deadlock3.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_deadlock1.out", " func: SUCCESS, main: FAIL")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_deadlock2.out", " func: SUCCESS, main: FAIL")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_deadlock3.out", " func: SUCCESS, main: FAIL")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/disj-no-deadlock1.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_disj_no_deadlock1.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/disj-no-deadlock2.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_disj_no_deadlock2.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/disj-no-deadlock3.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_disj_no_deadlock3.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/disj-deadlock.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_disj_deadlock.out", " func: SUCCESS, main: FAIL")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/ordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_ordered_locking.out", " func: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/unordered-locking.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_unordered_locking.out", " func: FAIL, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/oracle.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_oracle.out", " thread: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/owicki-gries.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_owicki_gries.out", " incrementor2: SUCCESS, incrementor1: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/threads/fibonacci.ss", "  --en-para -tp parahip --en-lsmu-infer --en-thrd-resource", "results", "threads_fibonacci.out", " seq_fib: SUCCESS, para_fib: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/trees.ss", "", "results", "trees.out", "insert: SUCCESS")
  }

  def addBagsTests(suite: HipTestSuite): Unit = {

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-all-1.ss", " ", "results", "avl_all_1.out", " remove_min:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-all.ss", " ", "results", "avl_all.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS, remove_max_add:  SUCCESS,  : remove_min_add, insert:  SUCCESS,  rotate_double_left:   SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-modular-2.ss", " ", "results", "avl_modular_2.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-modular-3.ss", " ", "results", "avl_modular_3.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS, remove_max_add:  SUCCESS,  : remove_min_add, insert:  SUCCESS,  rotate_double_left:   SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-modular-2.ss", " ", "results", "avl_modular_2.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS, height: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-modular-hei.ss", " ", "results", "avl_modular_hei.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  rotate_right2:  SUCCESS,  rotate_left2:  SUCCESS,  height: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-modular-new3.ss", " ", "results", "avl_modular_new3.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  is_mem:  SUCCESS, rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  diff_h_by_2:  SUCCESS,  diff_h_by_1:  SUCCESS,  eq_h:  SUCCESS,  less_h:  SUCCESS,  get_max_height_add1:  SUCCESS,  height: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-modular-set.ss", "", "results", "avl_modular_set.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-modular-siz.ss", " ", "results", "avl_modular_siz.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl-modular.ss", " ", "results", "avl_modular.out", " delete:  SUCCESS,  delete_top:  SUCCESS,  remove_min:  SUCCESS,  remove_max_add:  SUCCESS,  remove_min_add:  SUCCESS,  insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS,  height: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl.scp.ss", "", "results", "avl.scp.out", " delete:  SUCCESS,  remove_min:  SUCCESS,  insert_inline1:  SUCCESS,  insert_inline:  SUCCESS,  insert1:  SUCCESS,  insert:  SUCCESS, build_avl2:  SUCCESS,  build_avl1:  SUCCESS,  rotate_double_right1:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left1:  SUCCESS, rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right1:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left1:  SUCCESS,  rotate_left:  SUCCESS, height1:  SUCCESS,  height:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/avl.ss", " ", "results", "avl.out", " insert_inline:  SUCCESS,   insert:  SUCCESS,  rotate_double_right:  SUCCESS,  rotate_double_left:  SUCCESS,  get_max:  SUCCESS,  rotate_right:  SUCCESS,  rotate_left:  SUCCESS, height:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/bubble.ss", " ", "results", "bubble.out", " bsort1:  SUCCESS,  bubble1:  SUCCESS,  id1:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/cll.ss", " ", "results", "cll.out", " delete2:  SUCCESS,  delete:  SUCCESS,  count:  SUCCESS,  count_rest:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/dll.ss", " ", "results", "dll.out", " append:  SUCCESS,  insert:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/insertion.ss", " ", "results", "insertion.out", " insertion_sort:  SUCCESS,  delete:  SUCCESS,  insert:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/ll.ss", " ", "results", "ll.out", " reverse1:  SUCCESS,  delete1:  SUCCESS,  insert:  SUCCESS,  append:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/merge-modular.ss", " ", "results", "merge_modular.out", " insert1:  SUCCESS,  merge1:  SUCCESS,  merge_sort1:  SUCCESS,  split1:  SUCCESS,  count1:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/merge.ss", " ", "results", "merge.out", " insert1:  SUCCESS,  merge1:  SUCCESS,  merge_sort1:  SUCCESS,  split1:  SUCCESS,  count1:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/qsort.ss", " ", "results", "qsort.out", " qsort1:  SUCCESS,  append_bll1:  SUCCESS,  partition1:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/rb_bags.ss", " ", "results", "rb_bags.out", " insert_1:  SUCCESS,  del_1:  SUCCESS,  remove_min_1:  SUCCESS,  del_2r_1:  SUCCESS,  del_2_1:  SUCCESS,  del_3r_1:  SUCCESS, del_3_1:  SUCCESS,  del_4r_1:  SUCCESS,  del_4_1:  SUCCESS,  del_5r_1:  SUCCESS,  del_5_1:  SUCCESS,  del_6r_1:  SUCCESS,  del_6_1:  SUCCESS, is_black_1:  SUCCESS,  is_red_1:  SUCCESS,  case_2r_1:  SUCCESS,  rotate_case_3r_1:  SUCCESS,  case_2_1:  SUCCESS,  rotate_case_3_1:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/rb.scp.ss", " ", "results", "rb.scp.out", " insert_1:  SUCCESS,  insert:  SUCCESS,  del_1:  SUCCESS,  del:  SUCCESS,  remove_min_1:  SUCCESS,  remove_min:  SUCCESS,  del_2r_1:  SUCCESS,  del_2r:  SUCCESS,  del_2_1:  SUCCESS,  del_2:  SUCCESS,  del_3r_1:  SUCCESS,  del_3r:  SUCCESS,  del_3_1:  SUCCESS,  del_3:  SUCCESS,  del_4r_1:  SUCCESS,  del_4r:  SUCCESS,  del_4_1:  SUCCESS,  del_4:  SUCCESS,  del_5r_1:  SUCCESS,  del_5r:  SUCCESS,  del_5_1:  SUCCESS,  del_5:  SUCCESS,  del_6r_1:  SUCCESS,  del_6r:  SUCCESS,  del_6_1:  SUCCESS,  del_6:  SUCCESS,  is_black_1:  SUCCESS,  is_black:  SUCCESS,  is_red_1:  SUCCESS,  is_red:  SUCCESS,  case_2r_1:  SUCCESS,  case_2r:  SUCCESS,  rotate_case_3r_1:  SUCCESS,  rotate_case_3r:  SUCCESS,  case_2_1:  SUCCESS,  case_2:  SUCCESS,  rotate_case_3_1:  SUCCESS,  rotate_case_3:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/selection.ss", " ", "results", "selection.out", " selection_sort:  SUCCESS,  delete_min:  SUCCESS,  find_min:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/bags/trees.ss", " ", "results", "trees.out", " delete1:  SUCCESS,  remove_min1:  SUCCESS,  insert1:  SUCCESS,  flatten1:  SUCCESS,  append1:  SUCCESS")
  }
  def addHipBagaTests(suite: HipTestSuite): Unit = {
    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/eps.ss", " --baga-xpure", "results", "eps.out", " get_next: SUCCESS, get_next_next: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/append.ss", " --baga-xpure", "results", "append.out", " append: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/append-tail.ss", " --baga-xpure", "results", "append_tail.out", "append: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-bind.ss", " --baga-xpure", "results", "avl_bind.out", " height: SUCCESS,  rotate_left: SUCCESS,  rotate_right: SUCCESS,  get_max: SUCCESS,  rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl.ss", " --baga-xpure", "results", "avl.out", " height: SUCCESS, rotate_left: SUCCESS, rotate_right: SUCCESS, get_max: SUCCESS, rotate_double_left: SUCCESS, rotate_double_right: SUCCESS, build_avl1: SUCCESS, build_avl2: SUCCESS, insert: SUCCESS, insert_inline: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-orig-2.ss", " --baga-xpure", "results", "avl_orig_2.out", "height: SUCCESS, get_max: SUCCESS, insert: SUCCESS, double_left_child: SUCCESS, double_right_child: SUCCESS, rotate_left_child: SUCCESS,  rotate_right_child: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/avl-orig3.ss", " --baga-xpure", "results", "avl_orig3.out", " height: SUCCESS, get_max: SUCCESS, insert: SUCCESS,  double_left_child: SUCCESS, double_right_child: SUCCESS,  rotate_left_child: SUCCESS, rotate_right_child: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/bll.ss", " --baga-xpure", "results", "bll.out", " insert: SUCCESS, delete: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/bubble.ss", " --baga-xpure", "results", "bubble.out", " id2: SUCCESS, id3: SUCCESS, bubble: SUCCESS, bsort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/cll.ss", " --baga-xpure", "results", "cll.out", " test: SUCCESS, insert: SUCCESS, count_rest: SUCCESS, count: SUCCESS, delete: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/complete.ss", " --baga-xpure", "results", "complete.out", " maxim: SUCCESS, minim: SUCCESS, height: SUCCESS, min_height: SUCCESS, insert: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/dll.ss", " --baga-xpure", "results", "dll.out", " insert: SUCCESS, delete: SUCCESS, delete1: SUCCESS, test_del: SUCCESS, test_del2: SUCCESS, test_fold: SUCCESS, append: SUCCESS, append1: SUCCESS, f1: SUCCESS, f2: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/heaps.ss", " --baga-xpure", "results", "heaps.out", " insert: SUCCESS, deleteoneel: SUCCESS, deleteone: SUCCESS, ripple: SUCCESS, deletemax: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/insertion.ss", " --baga-xpure", "results", "insertion.out", " insert: SUCCESS, insertion_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/ll.ss", " --baga-xpure", "results", "ll.out", " append: SUCCESS, ret_first: SUCCESS, get_next: SUCCESS, set_next: SUCCESS, set_null: SUCCESS, get_next_next: SUCCESS, insert: SUCCESS, delete: SUCCESS, create_list: SUCCESS, reverse: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/merge.ss", " --baga-xpure", "results", "merge.out", " count: SUCCESS, split_func: SUCCESS, merge_sort: SUCCESS, merge: SUCCESS, insert: SUCCESSxw")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/perfect.ss", " --baga-xpure", "results", "perfect.out", " simple_insert: SUCCESS, create: SUCCESS, maxim: SUCCESS, height: SUCCESS, insert: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/qsort.ss", " --baga-xpure", "results", "qsort.out", " partition: SUCCESS, append_bll: SUCCESS, qsort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/selection.ss", " --baga-xpure", "results", "selection.out", " find_min: SUCCESS, delete_min: SUCCESS, selection_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/sll.ss", " --baga-xpure", "results", "sll.out", " insert: SUCCESS, insert2: SUCCESS, delete: SUCCESS, get_tail: SUCCESS, insertion_sort: SUCCESS, id: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/trees.ss", " --baga-xpure", "results", "trees.out", " append: SUCCESS, count: SUCCESS, flatten: SUCCESS, insert: SUCCESS, remove_min: SUCCESS, delete: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/rb.ss", " --baga-xpure", "results", "rb.out", " rotate_case_3: SUCCESS, case_2: SUCCESS, rotate_case_3r: SUCCESS, case_2r: SUCCESS, is_red: SUCCESS, is_black: SUCCESS, del_6: SUCCESS, del_6r: SUCCESS, del_5: SUCCESS, del_5r: SUCCESS, del_4: SUCCESS, del_4r: SUCCESS, del_3: SUCCESS, del_3r: SUCCESS, del_2: SUCCESS, remove_min: SUCCESS, del: SUCCESS, insert: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/global-ll.ss", " --baga-xpure", "results", "global_ll.out", " insert_rec: SUCCESS, delete_last_rec: SUCCESS, insert: SUCCESS, delete_last: SUCCESS, main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/dll-modular.ss", " --overeps --baga-xpure", "results", "modular_examples_dll_modular.out", "append:  SUCCESS,  append1:  SUCCESS,  append2:  SUCCESS, delete:  SUCCESS,  delete1:  SUCCESS,  f1:  SUCCESS, f2:  SUCCESS,  insert:  SUCCESS,  test_del:  SUCCESS, test_del2:  SUCCESS,  test_fold:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/selection-modular.ss", " --overeps --lda --baga-xpure", "results", "modular_examples_selection_modular.out", "delete_min:  SUCCESS,  find_min:  SUCCESS,  selection_sort:  SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip_baga/modular_examples/qsort-modular.ss", " --overeps --lda --baga-xpure", "results", "modular_examples_qsort_modular.out", "append_bll:  SUCCESS,  partition:  SUCCESS,  qsort:  SUCCESS")

  }

}