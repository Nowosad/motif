#ifndef get_polygons_H
#define get_polygons_H
#include <comat.h>
// [[Rcpp::depends(comat)]]
#include "create_attributes.h"
#include "get_composition.h"
#include "na_prop.h"
using namespace Rcpp;

Rcpp::List get_polygons(const Rcpp::List& input,
                  std::string type,
                  const arma::imat& m,
                  const arma::imat& directions,
                  Function f,
                  double threshold,
                  const std::string fun,
                  const std::string na_action,
                  Rcpp::List classes);

#endif // get_polygons_H
