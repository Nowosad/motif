#ifndef get_motifel_size_H
#define get_motifel_size_H
#include <comat.h>
// [[Rcpp::depends(comat)]]
#include "create_attributes.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

int get_motifel_size(int num_r, int num_c, int shift);

#endif // get_motifel_size_H
