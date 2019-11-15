#ifndef na_prop_H
#define na_prop_H
#include "RcppArmadillo.h"
#include <algorithm>     // for count_if
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

// [[Rcpp::export]]
double na_prop(const IntegerMatrix& x);

// [[Rcpp::export]]
double na_prop_arma(const arma::imat& x);

#endif // na_prop_H
