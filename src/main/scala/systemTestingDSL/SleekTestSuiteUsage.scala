package systemTestingDSL

import systemTestingDSL.matchers._
import systemTestingDSL.testSuite.SleekTestSuite
import com.typesafe.config.Config

class SleekTestSuiteUsage(configuration: Config) {

  val WORKING_DIR = configuration.getString("SLEEK_DIR")
  val OUTPUT_DIR = configuration.getString("SLEEK_OUTPUT_DIRECTORY")

  def run(): Unit = {

    val suite = new SleekTestSuite()

    suite addTest ("sleek", WORKING_DIR + "sleek.slk", " ", OUTPUT_DIR, "sleek", "Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "cll-d.slk", " ", OUTPUT_DIR, "cll_d", "Valid")

    suite addTest ("sleek", WORKING_DIR + "label-basic.slk", " --dis-eps", OUTPUT_DIR, "label_basic", "Fail, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "label-dll.slk", " --dis-eps", OUTPUT_DIR, "label_dll", "Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek1.slk", " ", OUTPUT_DIR, "sleek1", "Fail")

    suite addTest ("sleek", WORKING_DIR + "sleek10.slk", " ", OUTPUT_DIR, "sleek10", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "sleek2.slk", " ", OUTPUT_DIR, "sleek2", "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "sleek3.slk", " --elp", OUTPUT_DIR, "sleek3", "Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek4.slk", " ", OUTPUT_DIR, "sleek4", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek6.slk", " ", OUTPUT_DIR, "sleek6", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek7.slk", "  --dis-lem-gen ", OUTPUT_DIR, "sleek7", "Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek8.slk", "  --dis-lem-gen ", OUTPUT_DIR, "sleek8", "Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek8.slk", "  --elp ", OUTPUT_DIR, "sleek8", "Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek9.slk", "  --elp ", OUTPUT_DIR, "sleek9", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek12-lend.slk", " ", OUTPUT_DIR, "sleek12_lend", "Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "sleek13-lend.slk", " ", OUTPUT_DIR, "sleek13_lend", "Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "lst-under1.slk", " --inv-test", OUTPUT_DIR, "lst_under1", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "lst-under2.slk", " --inv-test", OUTPUT_DIR, "lst_under2", "Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "ll-under1a.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1a", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "ll-under1b.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1b", "Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "ll-under1c.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1c", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "ll-under1d.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1d", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "ll-under1e.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1e", "Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "ll-under1f.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1f", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "baga-test-eps.slk", " --eps", OUTPUT_DIR, "baga_test_eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "baga-test.slk", " ", OUTPUT_DIR, "baga_test", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "baga-test-2.slk", " --dis-baga-xpure --dis-eps", OUTPUT_DIR, "baga_test_2", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "baga-test-2.slk", " --baga-xpure", OUTPUT_DIR, "baga_test_2", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "symb-diff.slk", " ", OUTPUT_DIR, "symb_diff", "Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "xpure3nodes.slk", "", OUTPUT_DIR, "xpure3nodes", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/app-inv.slk", " --inv --dis-eps", OUTPUT_DIR, "infer_app_inv", "Valid, Valid, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/app-inv2.slk", " --inv --dis-eps", OUTPUT_DIR, "infer_app_inv2", "Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "infer/infer1.slk", " ", OUTPUT_DIR, "infer_infer1", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer2.slk", " ", OUTPUT_DIR, "infer_infer2", "Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "infer/infer4.slk", " ", OUTPUT_DIR, "infer_infer4", "Fail, Fail, Val")

    suite addTest ("sleek", WORKING_DIR + "infer/infer5.slk", " ", OUTPUT_DIR, "infer_infer5", "Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer5a.slk", " ", OUTPUT_DIR, "infer_infer5a", "Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer6.slk", " ", OUTPUT_DIR, "infer_infer6", "Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer7.slk", " ", OUTPUT_DIR, "infer_infer7", "Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer8.slk", " ", OUTPUT_DIR, "infer_infer8", "Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer9.slk", " ", OUTPUT_DIR, "infer_infer9", "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer10.slk", " ", OUTPUT_DIR, "infer_infer10", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer11.slk", " ", OUTPUT_DIR, "infer_infer11", "Fail")

    suite addTest ("sleek", WORKING_DIR + "infer/infer12.slk", " ", OUTPUT_DIR, "infer_infer12", "Valid, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer12.slk", " ", OUTPUT_DIR, "infer_infer12", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer13.slk", " --sa-en-cont", OUTPUT_DIR, "infer_infer13", "Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer14.slk", " --sa-en-pure-field", OUTPUT_DIR, "infer_infer14", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer15.slk", " ", OUTPUT_DIR, "infer_infer15", "Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infer/infer16.slk", " ", OUTPUT_DIR, "infer_infer16", "Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "ann2.slk", "  --imm --en-imm-inv --etcsu1 ", OUTPUT_DIR, "ann2", "Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm/imm1.slk", "  --imm --etcsu1 ", OUTPUT_DIR, "imm_imm1", "Fail, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "imm/imm2.slk", "  --imm --etcsu1 ", OUTPUT_DIR, "imm_imm2", "Fail, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm/imm3.slk", "  --imm --etcsu1 ", OUTPUT_DIR, "imm_imm3", "Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "imm/imm4.slk", "  --imm --etcsu1 ", OUTPUT_DIR, "imm_imm4", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm/imm-hard.slk", "  --imm --eps", OUTPUT_DIR, "imm_imm_hard", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "imm-field/sleek01.slk", "  --field-ann --etcsu1 ", OUTPUT_DIR, "imm_field_sleek01", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm-field/sleek02.slk", "  --field-ann --etcsu1 ", OUTPUT_DIR, "imm_field_sleek02", "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "imm-field/sleek03.slk", "  --field-ann --etcsu1 ", OUTPUT_DIR, "imm_field_sleek03", "Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "eps.slk", "  --dis-imm ", OUTPUT_DIR, "eps", "Valid")

    suite addTest ("sleek", WORKING_DIR + "imm-field/sleek05.slk", "  --field-ann --etcsu1 ", OUTPUT_DIR, "imm_field_sleek05", "Valid, Fail, Fail, Fail, Fail, Fail, Valid, Valid, Val")

    suite addTest ("sleek", WORKING_DIR + "classic/classic1.slk", " ", OUTPUT_DIR, "classic_classic1", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic1.slk", "  --classic", OUTPUT_DIR, "classic_classic1", "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic1a.slk", " ", OUTPUT_DIR, "classic_classic1a", "Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic1b.slk", " ", OUTPUT_DIR, "classic_classic1b", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic2.slk", " ", OUTPUT_DIR, "classic_classic2", "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic3.slk", " ", OUTPUT_DIR, "classic_classic3", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "classic/classic4.slk", " ", OUTPUT_DIR, "classic_classic4", "Valid, Fail, Valid, Fail, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "inf-no-eps.slk", "--dsd --en-inf --dis-eps", OUTPUT_DIR, "inf_no_eps", "Fail, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "infinity.slk", "--dsd --en-inf --dis-eps", OUTPUT_DIR, "infinity", "Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "inflem.slk", "  --en-inf --elp --dis-eps", OUTPUT_DIR, "inflem", "Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/sort2.slk", "  --elp --dis-lem-gen ", OUTPUT_DIR, "lemmas_sort2", "Fail, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lseg.slk", "  --elp --dis-lem-gen ", OUTPUT_DIR, "lemmas_lseg", "Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lseg1.slk", "  --elp --dis-lem-gen ", OUTPUT_DIR, "lemmas_lseg1", "Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/rlseg.slk", "  --elp --dis-lem-gen", OUTPUT_DIR, "lemmas_rlseg", "Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lemma-fold.slk", "  --elp ", OUTPUT_DIR, "lemmas_lemma_fold", "Valid, Valid, ")

    suite addTest ("sleek", WORKING_DIR + "lemmas/rd-lem-1.slk", "  --elp --dis-lem-gen ", OUTPUT_DIR, "lemmas_rd_lem_1", "Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/rd-lem-2.slk", " ", OUTPUT_DIR, "lemmas_rd_lem_2", "Fail")

    suite addTest ("sleek", WORKING_DIR + "lemmas/app-tail.slk", "  --elp ", OUTPUT_DIR, "lemmas_app_tail", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lseg_case.slk", "  --elp --dis-lem-gen ", OUTPUT_DIR, "lemmas_lseg_case", "Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/ll.slk", "  --elp ", OUTPUT_DIR, "lemmas_ll", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "lemmas/ll_tail.slk", "  --elp ", OUTPUT_DIR, "lemmas_ll_tail", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/nlseg3.slk", " ", OUTPUT_DIR, "lemmas_nlseg3", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/nlseg4e.slk", "  --elp ", OUTPUT_DIR, "lemmas_nlseg4e", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/nlseg4e1.slk", " --dis-imm", OUTPUT_DIR, "lemmas_nlseg4e1", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/sll_tailL.slk", "  --elp ", OUTPUT_DIR, "lemmas_sll_tailL", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/dseg-new.slk", "  --elp --dis-lem-gen ", OUTPUT_DIR, "lemmas_dseg_new", "Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/dseg1.slk", "  --elp --dis-lem-gen ", OUTPUT_DIR, "lemmas_dseg1", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "lemmas/odd-lseg.slk", "  --elp --dis-lem-gen --dis-eps", OUTPUT_DIR, "lemmas_odd_lseg", "Valid, Valid, Fail, Valid, Valid, Fail, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "lemmas/lseg_complex.slk", "  --elp --dis-lem-gen ", OUTPUT_DIR, "lemmas_lseg_complex", "Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/split_simple.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_split_simple", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/split-combine.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_split_combine", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "vperm/vperm.slk", " --ann-vp", OUTPUT_DIR, "vperm_vperm", "Valid, Valid, Fail, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid, Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm-split-combine.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_bperm_split_combine", "Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-static.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_static", "Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-dynamic2.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_dynamic2", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "threads/thrd1.slk", " --en-para --en-thrd-resource -tp redlog", OUTPUT_DIR, "threads_thrd1", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Val")

    suite addTest ("sleek", WORKING_DIR + "conchip/threads.slk", " -tp parahip", OUTPUT_DIR, "conchip_threads", "Valid, Valid, Val")

    suite addTest ("sleek", WORKING_DIR + "conchip/latch.slk", " -tp parahip", OUTPUT_DIR, "conchip_latch", "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fa")

    suite addTest ("sleek", WORKING_DIR + "../tree_shares/barrier.slk", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_barrier", "")

    suite addTest ("sleek", WORKING_DIR + "../tree_shares/barrier3.slk", " --eps --dis-field-ann --dis-precise-xpure -perm dperm", OUTPUT_DIR, ".._tree_shares_barrier3", "")

    suite addTest ("sleek", WORKING_DIR + "../tree_shares/barrier2.slk", " --eps --dis-field-ann -perm dperm", OUTPUT_DIR, ".._tree_shares_barrier2", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "../tree_shares/fractions.slk", " --eps --dis-field-ann -perm dperm", OUTPUT_DIR, ".._tree_shares_fractions", "Valid, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "threads/thrd1.slk", " --en-para --en-thrd-resource -tp redlog", OUTPUT_DIR, "threads_thrd1", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Val")

    suite addTest ("sleek", WORKING_DIR + "vperm/vperm.slk", " --ann-vp", OUTPUT_DIR, "vperm_vperm", "Valid, Valid, Fail, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "vperm/vperm2.slk", " --ann-vp", OUTPUT_DIR, "vperm_vperm2", "Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek", "Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek1.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek1", "Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek10.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek10", "Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek2.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek2", "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek3.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek3", "Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek4.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek4", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek6.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek6", "Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek7.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek7", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek8.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek8", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/sleek9.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_sleek9", "Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/norm1.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_norm1", "Fail, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/norm3.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_norm3", "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/norm4.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_norm4", "Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/uni_vars.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_uni_vars", "Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/frac1.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_frac1", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/frac2.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_frac2", "Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/frac3.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_frac3", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Fail, Fail")

    suite addTest ("sleek", WORKING_DIR + "fracperm/split_simple.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_split_simple", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/combine_data.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_combine_data", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/combine_simple.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_combine_simple", "Valid, Valid, Fail, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/split-combine.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_split_combine", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "fracperm/combine2.slk", "--en-para -perm fperm -tp redlog", OUTPUT_DIR, "fracperm_combine2", "Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm1.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_bperm1", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm-split.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_bperm_split", "Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm-combine.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_bperm_combine", "Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/bperm-split-combine.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_bperm_split_combine", "Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier1.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier1", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Fail")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-split.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_split", "Valid, Valid, Fail, Valid, Valid, Fail, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-combine.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_combine", "Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-sep.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_sep", "Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-static.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_static", "Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-dynamic.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_dynamic", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid")

    suite addTest ("sleek", WORKING_DIR + "veribsync/barrier-dynamic2.slk", "--en-para -perm bperm -tp redlog", OUTPUT_DIR, "veribsync_barrier_dynamic2", "Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid")

    //    suite.runAllTests
    //    suite generateTestStatistics
    regressionBuilderTests
  }

  def regressionBuilderTests(): Unit = {
    val regression = new RegressionTestReferenceBuilder(configuration).run
  }
  def inferenceTest: Unit = {
    val sleek2Test =
      new SleekTestCaseBuilder runCommand "sleek" onFile WORKING_DIR + "sleek2.slk" withArguments " " storeOutputInDirectory OUTPUT_DIR withOutputFileName "sleek2" checkAgainst "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail"
    val sleek2TestCase: SleekTestCase = sleek2Test.build
    sleek2TestCase.testInference(null)
  }
}
