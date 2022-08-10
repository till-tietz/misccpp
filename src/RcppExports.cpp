// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ergodic_sim
NumericMatrix ergodic_sim(int agents, int rounds, int money, double win, double loss);
RcppExport SEXP _misccpp_ergodic_sim(SEXP agentsSEXP, SEXP roundsSEXP, SEXP moneySEXP, SEXP winSEXP, SEXP lossSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type agents(agentsSEXP);
    Rcpp::traits::input_parameter< int >::type rounds(roundsSEXP);
    Rcpp::traits::input_parameter< int >::type money(moneySEXP);
    Rcpp::traits::input_parameter< double >::type win(winSEXP);
    Rcpp::traits::input_parameter< double >::type loss(lossSEXP);
    rcpp_result_gen = Rcpp::wrap(ergodic_sim(agents, rounds, money, win, loss));
    return rcpp_result_gen;
END_RCPP
}
// int_vec_insert
IntegerVector int_vec_insert(IntegerVector vec, IntegerVector vals, IntegerVector pos);
RcppExport SEXP _misccpp_int_vec_insert(SEXP vecSEXP, SEXP valsSEXP, SEXP posSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type vec(vecSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vals(valsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type pos(posSEXP);
    rcpp_result_gen = Rcpp::wrap(int_vec_insert(vec, vals, pos));
    return rcpp_result_gen;
END_RCPP
}
// int_mat_insert
IntegerMatrix int_mat_insert(IntegerMatrix m, IntegerVector col, IntegerVector row, IntegerVector vals);
RcppExport SEXP _misccpp_int_mat_insert(SEXP mSEXP, SEXP colSEXP, SEXP rowSEXP, SEXP valsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type col(colSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type row(rowSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type vals(valsSEXP);
    rcpp_result_gen = Rcpp::wrap(int_mat_insert(m, col, row, vals));
    return rcpp_result_gen;
END_RCPP
}
// mat_to_mat_insert
IntegerMatrix mat_to_mat_insert(IntegerMatrix old_m, IntegerMatrix new_m, IntegerVector new_rows, IntegerVector new_cols, IntegerVector old_rows, IntegerVector old_cols);
RcppExport SEXP _misccpp_mat_to_mat_insert(SEXP old_mSEXP, SEXP new_mSEXP, SEXP new_rowsSEXP, SEXP new_colsSEXP, SEXP old_rowsSEXP, SEXP old_colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type old_m(old_mSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type new_m(new_mSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type new_rows(new_rowsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type new_cols(new_colsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type old_rows(old_rowsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type old_cols(old_colsSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_to_mat_insert(old_m, new_m, new_rows, new_cols, old_rows, old_cols));
    return rcpp_result_gen;
END_RCPP
}
// mat_by_mat
NumericVector mat_by_mat(NumericMatrix m, IntegerVector row, IntegerVector col);
RcppExport SEXP _misccpp_mat_by_mat(SEXP mSEXP, SEXP rowSEXP, SEXP colSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type row(rowSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type col(colSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_by_mat(m, row, col));
    return rcpp_result_gen;
END_RCPP
}
// lt_permute
List lt_permute(DataFrame data);
RcppExport SEXP _misccpp_lt_permute(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(lt_permute(data));
    return rcpp_result_gen;
END_RCPP
}
// lt_gibbs
List lt_gibbs(DataFrame data, IntegerMatrix y_samp, IntegerVector strata, int n_strata, int n_waves, int total, int chain_samples, int chain_burnin, List priors, List param_init);
RcppExport SEXP _misccpp_lt_gibbs(SEXP dataSEXP, SEXP y_sampSEXP, SEXP strataSEXP, SEXP n_strataSEXP, SEXP n_wavesSEXP, SEXP totalSEXP, SEXP chain_samplesSEXP, SEXP chain_burninSEXP, SEXP priorsSEXP, SEXP param_initSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type y_samp(y_sampSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type strata(strataSEXP);
    Rcpp::traits::input_parameter< int >::type n_strata(n_strataSEXP);
    Rcpp::traits::input_parameter< int >::type n_waves(n_wavesSEXP);
    Rcpp::traits::input_parameter< int >::type total(totalSEXP);
    Rcpp::traits::input_parameter< int >::type chain_samples(chain_samplesSEXP);
    Rcpp::traits::input_parameter< int >::type chain_burnin(chain_burninSEXP);
    Rcpp::traits::input_parameter< List >::type priors(priorsSEXP);
    Rcpp::traits::input_parameter< List >::type param_init(param_initSEXP);
    rcpp_result_gen = Rcpp::wrap(lt_gibbs(data, y_samp, strata, n_strata, n_waves, total, chain_samples, chain_burnin, priors, param_init));
    return rcpp_result_gen;
END_RCPP
}
// rm_el
IntegerVector rm_el(IntegerVector input);
RcppExport SEXP _misccpp_rm_el(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type input(inputSEXP);
    rcpp_result_gen = Rcpp::wrap(rm_el(input));
    return rcpp_result_gen;
END_RCPP
}
// gen_pairs
IntegerMatrix gen_pairs(IntegerVector input);
RcppExport SEXP _misccpp_gen_pairs(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type input(inputSEXP);
    rcpp_result_gen = Rcpp::wrap(gen_pairs(input));
    return rcpp_result_gen;
END_RCPP
}
// extract
IntegerVector extract(NumericMatrix m, IntegerVector r, int c);
RcppExport SEXP _misccpp_extract(SEXP mSEXP, SEXP rSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< int >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(extract(m, r, c));
    return rcpp_result_gen;
END_RCPP
}
// pareto_sim
List pareto_sim(IntegerVector pop, IntegerVector mon, NumericVector prob, int iter);
RcppExport SEXP _misccpp_pareto_sim(SEXP popSEXP, SEXP monSEXP, SEXP probSEXP, SEXP iterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type pop(popSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type mon(monSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type prob(probSEXP);
    Rcpp::traits::input_parameter< int >::type iter(iterSEXP);
    rcpp_result_gen = Rcpp::wrap(pareto_sim(pop, mon, prob, iter));
    return rcpp_result_gen;
END_RCPP
}
// assign_cluster
List assign_cluster(NumericMatrix points, NumericMatrix centroids);
RcppExport SEXP _misccpp_assign_cluster(SEXP pointsSEXP, SEXP centroidsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type centroids(centroidsSEXP);
    rcpp_result_gen = Rcpp::wrap(assign_cluster(points, centroids));
    return rcpp_result_gen;
END_RCPP
}
// new_centroid
NumericMatrix new_centroid(NumericMatrix points, IntegerVector assigned_c);
RcppExport SEXP _misccpp_new_centroid(SEXP pointsSEXP, SEXP assigned_cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type assigned_c(assigned_cSEXP);
    rcpp_result_gen = Rcpp::wrap(new_centroid(points, assigned_c));
    return rcpp_result_gen;
END_RCPP
}
// kmean
List kmean(NumericMatrix points, NumericMatrix centroids, int k, int max_iter);
RcppExport SEXP _misccpp_kmean(SEXP pointsSEXP, SEXP centroidsSEXP, SEXP kSEXP, SEXP max_iterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type centroids(centroidsSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    rcpp_result_gen = Rcpp::wrap(kmean(points, centroids, k, max_iter));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_misccpp_ergodic_sim", (DL_FUNC) &_misccpp_ergodic_sim, 5},
    {"_misccpp_int_vec_insert", (DL_FUNC) &_misccpp_int_vec_insert, 3},
    {"_misccpp_int_mat_insert", (DL_FUNC) &_misccpp_int_mat_insert, 4},
    {"_misccpp_mat_to_mat_insert", (DL_FUNC) &_misccpp_mat_to_mat_insert, 6},
    {"_misccpp_mat_by_mat", (DL_FUNC) &_misccpp_mat_by_mat, 3},
    {"_misccpp_lt_permute", (DL_FUNC) &_misccpp_lt_permute, 1},
    {"_misccpp_lt_gibbs", (DL_FUNC) &_misccpp_lt_gibbs, 10},
    {"_misccpp_rm_el", (DL_FUNC) &_misccpp_rm_el, 1},
    {"_misccpp_gen_pairs", (DL_FUNC) &_misccpp_gen_pairs, 1},
    {"_misccpp_extract", (DL_FUNC) &_misccpp_extract, 3},
    {"_misccpp_pareto_sim", (DL_FUNC) &_misccpp_pareto_sim, 4},
    {"_misccpp_assign_cluster", (DL_FUNC) &_misccpp_assign_cluster, 2},
    {"_misccpp_new_centroid", (DL_FUNC) &_misccpp_new_centroid, 2},
    {"_misccpp_kmean", (DL_FUNC) &_misccpp_kmean, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_misccpp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
