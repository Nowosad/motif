// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// create_attributes
List create_attributes(List x);
RcppExport SEXP _lopata_create_attributes(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(create_attributes(x));
    return rcpp_result_gen;
END_RCPP
}
// get_motifel_size
int get_motifel_size(int num_r, int num_c, int shift);
RcppExport SEXP _lopata_get_motifel_size(SEXP num_rSEXP, SEXP num_cSEXP, SEXP shiftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type num_r(num_rSEXP);
    Rcpp::traits::input_parameter< int >::type num_c(num_cSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifel_size(num_r, num_c, shift));
    return rcpp_result_gen;
END_RCPP
}
// get_motifels_cocoma
List get_motifels_cocoma(IntegerMatrix x, IntegerMatrix y, const arma::imat directions, int size, int shift, double threshold, List classes);
RcppExport SEXP _lopata_get_motifels_cocoma(SEXP xSEXP, SEXP ySEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP thresholdSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_cocoma(x, y, directions, size, shift, threshold, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_motifels_coma
List get_motifels_coma(IntegerMatrix x, const arma::imat directions, int size, int shift, double threshold, List classes);
RcppExport SEXP _lopata_get_motifels_coma(SEXP xSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP thresholdSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_coma(x, directions, size, shift, threshold, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_motifels_fun
List get_motifels_fun(const List input, int size, int shift, Function f, double threshold, List classes);
RcppExport SEXP _lopata_get_motifels_fun(SEXP inputSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP fSEXP, SEXP thresholdSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< Function >::type f(fSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_fun(input, size, shift, f, threshold, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_motifels_ids
IntegerMatrix get_motifels_ids(int num_r, int num_c, int size, int shift);
RcppExport SEXP _lopata_get_motifels_ids(SEXP num_rSEXP, SEXP num_cSEXP, SEXP sizeSEXP, SEXP shiftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type num_r(num_rSEXP);
    Rcpp::traits::input_parameter< int >::type num_c(num_cSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_ids(num_r, num_c, size, shift));
    return rcpp_result_gen;
END_RCPP
}
// get_motifels_incoma
List get_motifels_incoma(const List input, const arma::imat directions, int size, int shift, double threshold, List classes);
RcppExport SEXP _lopata_get_motifels_incoma(SEXP inputSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP thresholdSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_incoma(input, directions, size, shift, threshold, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_motifels_wecoma
List get_motifels_wecoma(IntegerMatrix x, NumericMatrix w, const arma::imat directions, int size, int shift, double threshold, List classes, const std::string fun, const std::string na_action);
RcppExport SEXP _lopata_get_motifels_wecoma(SEXP xSEXP, SEXP wSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP thresholdSEXP, SEXP classesSEXP, SEXP funSEXP, SEXP na_actionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type w(wSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    Rcpp::traits::input_parameter< const std::string >::type fun(funSEXP);
    Rcpp::traits::input_parameter< const std::string >::type na_action(na_actionSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_wecoma(x, w, directions, size, shift, threshold, classes, fun, na_action));
    return rcpp_result_gen;
END_RCPP
}
// get_polygons_cocoma
List get_polygons_cocoma(const arma::imat& x, const arma::imat& y, const arma::imat& m, const arma::imat directions, double threshold, List classes);
RcppExport SEXP _lopata_get_polygons_cocoma(SEXP xSEXP, SEXP ySEXP, SEXP mSEXP, SEXP directionsSEXP, SEXP thresholdSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::imat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::imat& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::imat& >::type m(mSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_polygons_cocoma(x, y, m, directions, threshold, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_polygons_coma
List get_polygons_coma(const arma::imat& x, const arma::imat& m, const arma::imat directions, double threshold, List classes);
RcppExport SEXP _lopata_get_polygons_coma(SEXP xSEXP, SEXP mSEXP, SEXP directionsSEXP, SEXP thresholdSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::imat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::imat& >::type m(mSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_polygons_coma(x, m, directions, threshold, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_polygons_fun
List get_polygons_fun(const List input, const arma::imat& m, Function f, double threshold, List classes);
RcppExport SEXP _lopata_get_polygons_fun(SEXP inputSEXP, SEXP mSEXP, SEXP fSEXP, SEXP thresholdSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< const arma::imat& >::type m(mSEXP);
    Rcpp::traits::input_parameter< Function >::type f(fSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_polygons_fun(input, m, f, threshold, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_polygons_incoma
List get_polygons_incoma(const List input, const arma::imat& m, const arma::imat directions, double threshold, List classes);
RcppExport SEXP _lopata_get_polygons_incoma(SEXP inputSEXP, SEXP mSEXP, SEXP directionsSEXP, SEXP thresholdSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< const arma::imat& >::type m(mSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_polygons_incoma(input, m, directions, threshold, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_polygons_wecoma
List get_polygons_wecoma(const arma::imat& x, const arma::dmat& w, const arma::imat& m, const arma::imat directions, double threshold, const std::string fun, const std::string na_action, List classes);
RcppExport SEXP _lopata_get_polygons_wecoma(SEXP xSEXP, SEXP wSEXP, SEXP mSEXP, SEXP directionsSEXP, SEXP thresholdSEXP, SEXP funSEXP, SEXP na_actionSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::imat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::dmat& >::type w(wSEXP);
    Rcpp::traits::input_parameter< const arma::imat& >::type m(mSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< const std::string >::type fun(funSEXP);
    Rcpp::traits::input_parameter< const std::string >::type na_action(na_actionSEXP);
    Rcpp::traits::input_parameter< List >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(get_polygons_wecoma(x, w, m, directions, threshold, fun, na_action, classes));
    return rcpp_result_gen;
END_RCPP
}
// get_unique_values
std::vector<int> get_unique_values(const Rcpp::IntegerVector& x, bool na_omit);
RcppExport SEXP _lopata_get_unique_values(SEXP xSEXP, SEXP na_omitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type na_omit(na_omitSEXP);
    rcpp_result_gen = Rcpp::wrap(get_unique_values(x, na_omit));
    return rcpp_result_gen;
END_RCPP
}
// na_prop
double na_prop(const IntegerMatrix& x);
RcppExport SEXP _lopata_na_prop(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(na_prop(x));
    return rcpp_result_gen;
END_RCPP
}
// na_prop_polygon
double na_prop_polygon(const arma::imat& x, const double& no_of_outside_cells);
RcppExport SEXP _lopata_na_prop_polygon(SEXP xSEXP, SEXP no_of_outside_cellsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::imat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const double& >::type no_of_outside_cells(no_of_outside_cellsSEXP);
    rcpp_result_gen = Rcpp::wrap(na_prop_polygon(x, no_of_outside_cells));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_lopata_create_attributes", (DL_FUNC) &_lopata_create_attributes, 1},
    {"_lopata_get_motifel_size", (DL_FUNC) &_lopata_get_motifel_size, 3},
    {"_lopata_get_motifels_cocoma", (DL_FUNC) &_lopata_get_motifels_cocoma, 7},
    {"_lopata_get_motifels_coma", (DL_FUNC) &_lopata_get_motifels_coma, 6},
    {"_lopata_get_motifels_fun", (DL_FUNC) &_lopata_get_motifels_fun, 6},
    {"_lopata_get_motifels_ids", (DL_FUNC) &_lopata_get_motifels_ids, 4},
    {"_lopata_get_motifels_incoma", (DL_FUNC) &_lopata_get_motifels_incoma, 6},
    {"_lopata_get_motifels_wecoma", (DL_FUNC) &_lopata_get_motifels_wecoma, 9},
    {"_lopata_get_polygons_cocoma", (DL_FUNC) &_lopata_get_polygons_cocoma, 6},
    {"_lopata_get_polygons_coma", (DL_FUNC) &_lopata_get_polygons_coma, 5},
    {"_lopata_get_polygons_fun", (DL_FUNC) &_lopata_get_polygons_fun, 5},
    {"_lopata_get_polygons_incoma", (DL_FUNC) &_lopata_get_polygons_incoma, 5},
    {"_lopata_get_polygons_wecoma", (DL_FUNC) &_lopata_get_polygons_wecoma, 8},
    {"_lopata_get_unique_values", (DL_FUNC) &_lopata_get_unique_values, 2},
    {"_lopata_na_prop", (DL_FUNC) &_lopata_na_prop, 1},
    {"_lopata_na_prop_polygon", (DL_FUNC) &_lopata_na_prop_polygon, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_lopata(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
