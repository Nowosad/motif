#ifndef get_unique_values_H
#define get_unique_values_H
#include "Rcpp.h"
// [[Rcpp::plugins(cpp11)]]

std::vector<int> get_unique_values(const Rcpp::IntegerVector &x, bool na_omit = true);

#endif // get_unique_values_H
