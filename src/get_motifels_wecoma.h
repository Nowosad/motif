#ifndef get_motifels_wecoma_H
#define get_motifels_wecoma_H
#include <comat.h>
// [[Rcpp::depends(comat)]]
#include "create_attributes.h"
#include "get_motifel_size.h"
#include "na_prop.h"
using namespace Rcpp;

Rcpp::List get_motifels_wecoma(Rcpp::IntegerMatrix x,
                         Rcpp::NumericMatrix w,
                         const arma::imat directions,
                         int size,
                         int shift,
                         const std::string fun = "mean",
                         const std::string na_action = "replace");

#endif // get_motifels_wecoma_H
