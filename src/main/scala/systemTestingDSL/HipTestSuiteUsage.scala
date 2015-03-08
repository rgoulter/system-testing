package systemTestingDSL

import systemTestingDSL.testSuite.HipTestSuite

object HipTestSuiteUsage {
  def run(): Unit = {
    val suite = new HipTestSuite()

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_at.java", "", "results", "arr_at.out", "main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_binarysearch.java", "", "results", "arr_binarysearch.out", "binary_search: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_search_decrease_less_than_two.java", "", "results", "arr_search_decrease_less_than_two.out", "searchzero: FAIL")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_bubblesort.java", "", "results", "arr_bubblesort.out", "bubblesort: SUCCESS, bubble: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_bubblesort_perm.java", "", "results", "arr_bubblesort_perm.out", "bubblesort: SUCCESS, bubble: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_double.java", "", "results", "arr_double.out", "doublearr: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_extract_nonzeros.java", "", "results", "arr_extract_nonzeros.out", "copy_nonzeros: SUCCESS, count_nonzeros: SUCCESS, extract_nonzeros: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_init.java", "", "results", "arr_init.out", "zinit: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_insertsort.java", "", "results", "arr_insertsort.out", "insertelm: SUCCESS, insertion_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_insertsort_perm.java", "", "results", "arr_insertsort_perm.out", "insertelm: SUCCESS, insertion_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_invert.java", "", "results", "arr_invert.out", "Invert: SUCCESS, InvertHelper: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_max.java", "", "results", "arr_max.out", "max_value_of_array: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_mergesort.java", "", "results", "arr_mergesort.out", "merge_sorted_arrays: SUCCESS, copy_array: SUCCESS, merge_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_new_exp.java", "", "results", "arr_new_exp.out", "main: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_nqueens.java", "", "results", "arr_nqueens.out", "nQueens: SUCCESS, nQueensHelper: SUCCESS, nQueensHelperHelper: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_qsort.java", "", "results", "arr_qsort.out", "arraypart: SUCCESS, qsort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_rev.java", "", "results", "arr_rev.out", "arrayrev: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_selectionsort.java", "", "results", "arr_selectionsort.out", "array_index_of_max: SUCCESS, selection_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_selectionsort_perm.java", "", "results", "arr_selectionsort_perm.out", "array_index_of_max: SUCCESS, selection_sort: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_sparse.java", "--imm", "results", "arr_sparse.out", "create: SUCCESS, get: SUCCESS, setsa: SUCCESS")

    suite addTest ("hip", "/home/rohit/hg/sleek_hip/examples/working/hip/arr_sum.java", "", "results", "arr_sum.out", "sigmaright: SUCCESS, sigmaleft: SUCCESS")

    suite.runAllTests
    suite generateTestStatistics
  }
}