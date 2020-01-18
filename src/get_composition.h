#ifndef get_composition_H
#define get_composition_H

#include <RcppArmadillo.h>
#include "get_unique_values.h"
#include <vector>
#include <map>
using namespace Rcpp;

std::map<int, unsigned> get_class_index_map(const std::vector<int> &classes);

Rcpp::IntegerVector get_composition(const IntegerMatrix& x, std::vector<int> classes);

Rcpp::IntegerVector get_composition_list(Rcpp::List x, std::vector<int> classes);

#endif // get_composition_H
