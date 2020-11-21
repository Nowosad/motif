#ifndef na_prop_H
#define na_prop_H
#include "RcppArmadillo.h"
#include <algorithm>     // for count_if
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

// [[Rcpp::export]]
double na_prop(const IntegerMatrix& x, int size);

// [[Rcpp::export]]
double na_prop_polygon(const arma::imat& x, const double& no_of_outside_cells);

#endif // na_prop_H
