#ifndef get_motifels_coma_H
#define get_motifels_coma_H
#include <comat.h>
// [[Rcpp::depends(comat)]]
#include "create_attributes.h"
#include "get_motifel_size.h"

Rcpp::List get_motifels_coma(Rcpp::IntegerMatrix x,
                       const arma::imat directions,
                       int size,
                       int shift);

#endif // get_motifels_coma_H
