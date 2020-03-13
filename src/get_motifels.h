#ifndef get_motifels_H
#define get_motifels_H
#include <comat.h>
// [[Rcpp::depends(comat)]]
#include "create_attributes.h"
#include "get_composition.h"
#include "get_motifel_size.h"
#include "na_prop.h"
using namespace Rcpp;

Rcpp::List get_motifels(const Rcpp::List input,
                        std::string type,
                        const arma::imat directions,
                        int size,
                        int shift,
                        Function f,
                        double threshold,
                        List classes,
                        const std::string fun,
                        const std::string na_action);

#endif // get_motifels_H
