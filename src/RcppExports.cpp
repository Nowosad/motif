// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/lopata.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

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
static SEXP _lopata_get_motifel_size_try(SEXP num_rSEXP, SEXP num_cSEXP, SEXP shiftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< int >::type num_r(num_rSEXP);
    Rcpp::traits::input_parameter< int >::type num_c(num_cSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifel_size(num_r, num_c, shift));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _lopata_get_motifel_size(SEXP num_rSEXP, SEXP num_cSEXP, SEXP shiftSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_lopata_get_motifel_size_try(num_rSEXP, num_cSEXP, shiftSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// get_motifels_coma
List get_motifels_coma(IntegerMatrix x, const arma::imat directions, int size, int shift, const std::string fun, const std::string na_action);
static SEXP _lopata_get_motifels_coma_try(SEXP xSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP funSEXP, SEXP na_actionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< const std::string >::type fun(funSEXP);
    Rcpp::traits::input_parameter< const std::string >::type na_action(na_actionSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_coma(x, directions, size, shift, fun, na_action));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _lopata_get_motifels_coma(SEXP xSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP funSEXP, SEXP na_actionSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_lopata_get_motifels_coma_try(xSEXP, directionsSEXP, sizeSEXP, shiftSEXP, funSEXP, na_actionSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// get_motifels_wecoma
List get_motifels_wecoma(IntegerMatrix x, NumericMatrix w, const arma::imat directions, int size, int shift, const std::string fun, const std::string na_action);
static SEXP _lopata_get_motifels_wecoma_try(SEXP xSEXP, SEXP wSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP funSEXP, SEXP na_actionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type w(wSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< const std::string >::type fun(funSEXP);
    Rcpp::traits::input_parameter< const std::string >::type na_action(na_actionSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_wecoma(x, w, directions, size, shift, fun, na_action));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _lopata_get_motifels_wecoma(SEXP xSEXP, SEXP wSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP funSEXP, SEXP na_actionSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_lopata_get_motifels_wecoma_try(xSEXP, wSEXP, directionsSEXP, sizeSEXP, shiftSEXP, funSEXP, na_actionSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// get_motifels_cocoma
List get_motifels_cocoma(IntegerMatrix x, IntegerMatrix y, const arma::imat directions, int size, int shift);
static SEXP _lopata_get_motifels_cocoma_try(SEXP xSEXP, SEXP ySEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_cocoma(x, y, directions, size, shift));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _lopata_get_motifels_cocoma(SEXP xSEXP, SEXP ySEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_lopata_get_motifels_cocoma_try(xSEXP, ySEXP, directionsSEXP, sizeSEXP, shiftSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// get_motifels_incoma
List get_motifels_incoma(const List input, const arma::imat directions, int size, int shift, const std::string fun, const std::string na_action);
static SEXP _lopata_get_motifels_incoma_try(SEXP inputSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP funSEXP, SEXP na_actionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const List >::type input(inputSEXP);
    Rcpp::traits::input_parameter< const arma::imat >::type directions(directionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< const std::string >::type fun(funSEXP);
    Rcpp::traits::input_parameter< const std::string >::type na_action(na_actionSEXP);
    rcpp_result_gen = Rcpp::wrap(get_motifels_incoma(input, directions, size, shift, fun, na_action));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _lopata_get_motifels_incoma(SEXP inputSEXP, SEXP directionsSEXP, SEXP sizeSEXP, SEXP shiftSEXP, SEXP funSEXP, SEXP na_actionSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_lopata_get_motifels_incoma_try(inputSEXP, directionsSEXP, sizeSEXP, shiftSEXP, funSEXP, na_actionSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _lopata_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("int(*get_motifel_size)(int,int,int)");
        signatures.insert("List(*get_motifels_coma)(IntegerMatrix,const arma::imat,int,int,const std::string,const std::string)");
        signatures.insert("List(*get_motifels_wecoma)(IntegerMatrix,NumericMatrix,const arma::imat,int,int,const std::string,const std::string)");
        signatures.insert("List(*get_motifels_cocoma)(IntegerMatrix,IntegerMatrix,const arma::imat,int,int)");
        signatures.insert("List(*get_motifels_incoma)(const List,const arma::imat,int,int,const std::string,const std::string)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _lopata_RcppExport_registerCCallable() { 
    R_RegisterCCallable("lopata", "_lopata_get_motifel_size", (DL_FUNC)_lopata_get_motifel_size_try);
    R_RegisterCCallable("lopata", "_lopata_get_motifels_coma", (DL_FUNC)_lopata_get_motifels_coma_try);
    R_RegisterCCallable("lopata", "_lopata_get_motifels_wecoma", (DL_FUNC)_lopata_get_motifels_wecoma_try);
    R_RegisterCCallable("lopata", "_lopata_get_motifels_cocoma", (DL_FUNC)_lopata_get_motifels_cocoma_try);
    R_RegisterCCallable("lopata", "_lopata_get_motifels_incoma", (DL_FUNC)_lopata_get_motifels_incoma_try);
    R_RegisterCCallable("lopata", "_lopata_RcppExport_validate", (DL_FUNC)_lopata_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_lopata_create_attributes", (DL_FUNC) &_lopata_create_attributes, 1},
    {"_lopata_get_motifel_size", (DL_FUNC) &_lopata_get_motifel_size, 3},
    {"_lopata_get_motifels_coma", (DL_FUNC) &_lopata_get_motifels_coma, 6},
    {"_lopata_get_motifels_wecoma", (DL_FUNC) &_lopata_get_motifels_wecoma, 7},
    {"_lopata_get_motifels_cocoma", (DL_FUNC) &_lopata_get_motifels_cocoma, 5},
    {"_lopata_get_motifels_incoma", (DL_FUNC) &_lopata_get_motifels_incoma, 6},
    {"_lopata_RcppExport_registerCCallable", (DL_FUNC) &_lopata_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_lopata(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
