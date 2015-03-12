package systemTestingDSL

import systemTestingDSL.matchers._
import systemTestingDSL.testSuite.SleekTestSuite
import com.typesafe.config.Config

class SleekTestSuiteUsage(configuration: Config) {

  val WORKING_DIR = configuration.getString("SLEEK_DIR")

  def run(): Unit = {

    val suite = new SleekTestSuite()

    suite addTest ("sleek", WORKING_DIR + "sleek.slk", " ", "results", "sleek", "Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "cll-d.slk", " ", "results", "cll_d", "Valid")

    suite addTest ("sleek", WORKING_DIR + "label-basic.slk", " --dis-eps", "results", "label_basic", "Fail, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "label-dll.slk", " --dis-eps", "results", "label_dll", "Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek1.slk", " ", "results", "sleek1", "Fail")

    suite addTest ("sleek", WORKING_DIR + "sleek10.slk", " ", "results", "sleek10", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "sleek2.slk", " ", "results", "sleek2", "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "sleek3.slk", " --elp", "results", "sleek3", "Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek4.slk", " ", "results", "sleek4", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek6.slk", " ", "results", "sleek6", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek7.slk", "  --dis-lem-gen ", "results", "sleek7", "Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek8.slk", "  --dis-lem-gen ", "results", "sleek8", "Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek8.slk", "  --elp ", "results", "sleek8", "Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek9.slk", "  --elp ", "results", "sleek9", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek12-lend.slk", " ", "results", "sleek12_lend", "Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek13-lend.slk", " ", "results", "sleek13_lend", "Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "lst-under1.slk", " --inv-test", "results", "lst_under1", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "lst-under2.slk", " --inv-test", "results", "lst_under2", "Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "ll-under1a.slk", "  --inv-test --baga-xpure ", "results", "ll_under1a", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "ll-under1b.slk", "  --inv-test --baga-xpure ", "results", "ll_under1b", "Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "ll-under1c.slk", "  --inv-test --baga-xpure ", "results", "ll_under1c", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "ll-under1d.slk", "  --inv-test --baga-xpure ", "results", "ll_under1d", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "ll-under1e.slk", "  --inv-test --baga-xpure ", "results", "ll_under1e", "Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "ll-under1f.slk", "  --inv-test --baga-xpure ", "results", "ll_under1f", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "baga-test-eps.slk", " --eps", "results", "baga_test_eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "baga-test.slk", " ", "results", "baga_test", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "baga-test-2.slk", " --dis-baga-xpure --dis-eps", "results", "baga_test_2", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "baga-test-2.slk", " --baga-xpure", "results", "baga_test_2", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "symb-diff.slk", " ", "results", "symb_diff", "Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "xpure3nodes.slk", "", "results", "xpure3nodes", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/app-inv.slk", " --inv --dis-eps", "results", "infer_app_inv", "Valid, Valid, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/app-inv2.slk", " --inv --dis-eps", "results", "infer_app_inv2", "Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "infer/infer1.slk", " ", "results", "infer_infer1", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer2.slk", " ", "results", "infer_infer2", "Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "infer/infer4.slk", " ", "results", "infer_infer4", "Fail, Fail, Val")

    suite addTest ("sleek", WORKING_DIR + "infer/infer5.slk", " ", "results", "infer_infer5", "Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer5a.slk", " ", "results", "infer_infer5a", "Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer6.slk", " ", "results", "infer_infer6", "Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer7.slk", " ", "results", "infer_infer7", "Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer8.slk", " ", "results", "infer_infer8", "Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer9.slk", " ", "results", "infer_infer9", "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer10.slk", " ", "results", "infer_infer10", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer11.slk", " ", "results", "infer_infer11", "Fail")

    suite addTest ("sleek", WORKING_DIR + "infer/infer12.slk", " ", "results", "infer_infer12", "Valid, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer12.slk", " ", "results", "infer_infer12", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer13.slk", " --sa-en-cont", "results", "infer_infer13", "Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer14.slk", " --sa-en-pure-field", "results", "infer_infer14", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer15.slk", " ", "results", "infer_infer15", "Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer16.slk", " ", "results", "infer_infer16", "Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "ann2.slk", "  --imm --en-imm-inv --etcsu1 ", "results", "ann2", "Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm/imm1.slk", "  --imm --etcsu1 ", "results", "imm_imm1", "Fail, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "imm/imm2.slk", "  --imm --etcsu1 ", "results", "imm_imm2", "Fail, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm/imm3.slk", "  --imm --etcsu1 ", "results", "imm_imm3", "Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "imm/imm4.slk", "  --imm --etcsu1 ", "results", "imm_imm4", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm/imm-hard.slk", "  --imm --eps", "results", "imm_imm_hard", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "imm-field/sleek01.slk", "  --field-ann --etcsu1 ", "results", "imm_field_sleek01", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm-field/sleek02.slk", "  --field-ann --etcsu1 ", "results", "imm_field_sleek02", "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm-field/sleek03.slk", "  --field-ann --etcsu1 ", "results", "imm_field_sleek03", "Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "eps.slk", "  --dis-imm ", "results", "eps", "Valid")

    suite addTest ("sleek", WORKING_DIR + "imm-field/sleek05.slk", "  --field-ann --etcsu1 ", "results", "imm_field_sleek05", "Valid, Fail, Fail, Fail, Fail, Fail, Valid, Valid, Val")

    suite addTest ("sleek", WORKING_DIR + "classic/classic1.slk", " ", "results", "classic_classic1", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic1.slk", "  --classic", "results", "classic_classic1", "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic1a.slk", " ", "results", "classic_classic1a", "Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic1b.slk", " ", "results", "classic_classic1b", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic2.slk", " ", "results", "classic_classic2", "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic3.slk", " ", "results", "classic_classic3", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic4.slk", " ", "results", "classic_classic4", "Valid, Fail, Valid, Fail, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "inf-no-eps.slk", "--dsd --en-inf --dis-eps", "results", "inf_no_eps", "Fail, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infinity.slk", "--dsd --en-inf --dis-eps", "results", "infinity", "Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "inflem.slk", "  --en-inf --elp --dis-eps", "results", "inflem", "Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/sort2.slk", "  --elp --dis-lem-gen ", "results", "lemmas_sort2", "Fail, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lseg.slk", "  --elp --dis-lem-gen ", "results", "lemmas_lseg", "Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lseg1.slk", "  --elp --dis-lem-gen ", "results", "lemmas_lseg1", "Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/rlseg.slk", "  --elp --dis-lem-gen", "results", "lemmas_rlseg", "Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lemma-fold.slk", "  --elp ", "results", "lemmas_lemma_fold", "Valid, Valid, ")

    suite addTest ("sleek", WORKING_DIR + "lemmas/rd-lem-1.slk", "  --elp --dis-lem-gen ", "results", "lemmas_rd_lem_1", "Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/rd-lem-2.slk", " ", "results", "lemmas_rd_lem_2", "Fail")

    suite addTest ("sleek", WORKING_DIR + "lemmas/app-tail.slk", "  --elp ", "results", "lemmas_app_tail", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lseg_case.slk", "  --elp --dis-lem-gen ", "results", "lemmas_lseg_case", "Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/ll.slk", "  --elp ", "results", "lemmas_ll", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "lemmas/ll_tail.slk", "  --elp ", "results", "lemmas_ll_tail", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/nlseg3.slk", " ", "results", "lemmas_nlseg3", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/nlseg4e.slk", "  --elp ", "results", "lemmas_nlseg4e", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/nlseg4e1.slk", " --dis-imm", "results", "lemmas_nlseg4e1", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/sll_tailL.slk", "  --elp ", "results", "lemmas_sll_tailL", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/dseg-new.slk", "  --elp --dis-lem-gen ", "results", "lemmas_dseg_new", "Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/dseg1.slk", "  --elp --dis-lem-gen ", "results", "lemmas_dseg1", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "lemmas/odd-lseg.slk", "  --elp --dis-lem-gen --dis-eps", "results", "lemmas_odd_lseg", "Valid, Valid, Fail, Valid, Valid, Fail, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lseg_complex.slk", "  --elp --dis-lem-gen ", "results", "lemmas_lseg_complex", "Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/split_simple.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_split_simple", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/split-combine.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_split_combine", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "vperm/vperm.slk", " --ann-vp", "results", "vperm_vperm", "Valid, Valid, Fail, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid, Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm-split-combine.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_bperm_split_combine", "Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-static.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier_static", "Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-dynamic2.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier_dynamic2", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "threads/thrd1.slk", " --en-para --en-thrd-resource -tp redlog", "results", "threads_thrd1", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Val")

    suite addTest ("sleek", WORKING_DIR + "conchip/threads.slk", " -tp parahip", "results", "conchip_threads", "Valid, Valid, Val")

    suite addTest ("sleek", WORKING_DIR + "conchip/latch.slk", " -tp parahip", "results", "conchip_latch", "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fa")

    suite addTest ("sleek", WORKING_DIR + "../tree_shares/barrier.slk", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", "results", ".._tree_shares_barrier", "")

    suite addTest ("sleek", WORKING_DIR + "../tree_shares/barrier3.slk", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", "results", ".._tree_shares_barrier3", "")

    suite addTest ("sleek", WORKING_DIR + "../tree_shares/barrier2.slk", " --eps --dis-field-ann -perm dperm", "results", ".._tree_shares_barrier2", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "../tree_shares/fractions.slk", " --eps --dis-field-ann -perm dperm", "results", ".._tree_shares_fractions", "Valid, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "threads/thrd1.slk", " --en-para --en-thrd-resource -tp redlog", "results", "threads_thrd1", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Val")

    suite addTest ("sleek", WORKING_DIR + "vperm/vperm.slk", " --ann-vp", "results", "vperm_vperm", "Valid, Valid, Fail, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "vperm/vperm2.slk", " --ann-vp", "results", "vperm_vperm2", "Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek", "Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek1.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek1", "Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek10.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek10", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek2.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek2", "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek3.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek3", "Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek4.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek4", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek6.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek6", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek7.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek7", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek8.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek8", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek9.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_sleek9", "Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/norm1.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_norm1", "Fail, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/norm3.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_norm3", "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/norm4.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_norm4", "Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/uni_vars.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_uni_vars", "Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/frac1.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_frac1", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/frac2.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_frac2", "Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/frac3.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_frac3", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/split_simple.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_split_simple", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/combine_data.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_combine_data", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/combine_simple.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_combine_simple", "Valid, Valid, Fail, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/split-combine.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_split_combine", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/combine2.slk", "--en-para -perm fperm -tp redlog", "results", "fracperm_combine2", "Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm1.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_bperm1", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm-split.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_bperm_split", "Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm-combine.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_bperm_combine", "Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm-split-combine.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_bperm_split_combine", "Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier1.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier1", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-split.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier_split", "Valid, Valid, Fail, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-combine.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier_combine", "Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-sep.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier_sep", "Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-static.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier_static", "Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-dynamic.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier_dynamic", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-dynamic2.slk", "--en-para -perm bperm -tp redlog", "results", "veribsync_barrier_dynamic2", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid")

    suite.runAllTests
    suite generateTestStatistics
  }

  def inferenceTest: Unit = {
    val sleek2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile WORKING_DIR + "sleek2.slk" withArguments " " storeOutputInDirectory "results" withOutputFileName "sleek2" checkAgainst "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail"
    val sleek2TestCase: SleekTestCase = sleek2Test.build
    sleek2TestCase.testInference(null)
  }
}
