#include <RcppArmadillo.h>
#include <comat.h>
#include "na_prop.h"
// [[Rcpp::depends(comat)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::imat get_polygons_coma(arma::imat x,
                             arma::imat y,
                             const arma::imat directions,
                             double threshold) {

  arma::imat classes_y = unique(y);
  classes_y = classes_y.elem(find(classes_y != NA_INTEGER));
  int classes_y_size = classes_y.size();
  // for (int i = 0; i < classes_y_size; i++){

  arma::uvec q;
  arma::uvec q2;
  List result(classes_y_size);

  // arma::uvec xx = 1;

  arma::umat u;

  arma::imat z;
  arma::imat zy;

  double na_perc;

  // arma::uvec u;
  for (int i = 0; i < classes_y_size; i++){
    std::cout << "class:   "  << classes_y[i]<< "\n";
    q = find(y == classes_y[i]);
    // std::cout << q << "\n";

    u = arma::ind2sub(size(y), q);
    arma::ucolvec min_coords = arma::min(u, 1);
    arma::ucolvec max_coords = arma::max(u, 1);

    unsigned int min_row = min_coords(0);
    unsigned int max_row = max_coords(0);
    unsigned int min_col = min_coords(1);
    unsigned int max_col = max_coords(1);

    z = x.submat(min_row, min_col, max_row, max_col);
    na_perc = na_prop(wrap(z));

    zy = y.submat(min_row, min_col, max_row, max_col);

    std::cout << "na prop:   " << na_perc << std::endl;

    q2 = find(zy != classes_y[i]);

    std::cout << "q2:   " << q2.size() << std::endl;


    arma::ivec replacer(q2.size());
    replacer.fill(NA_INTEGER);
    z.elem(q2) = replacer;


    //
    // std::cout << "min row:   " << min_coords(0) << std::endl;
    // std::cout << "max col:   " << max_coords(1) << std::endl;
    // arma::irowvec xx = x.elem(q);
    // result[i] = as<IntegerMatrix>(wrap(xx));
  }
  return result;
}

/***R
library(raster)
a = as.matrix(raster("inst/raster/landcover.tif"))
b = as.matrix(raster("inst/raster/ecoregions.tif"))

set.seed(1111)
a = matrix(sample(c(NA, 1:3), size = 20, replace = TRUE), ncol = 4)
b = matrix(c(2, 2, 2, 2, 3, 2, 2, 2, 3, 4, NA, NA, 4, 4, 4, 4, 4, 4, 4, 4), ncol = 4)
u = get_polygons_coma(a, b, directions = matrix(4), threshold = 0.5)
u
lopata::lop_coma(raster::raster(u), threshold = 0.9)$matrix

*/
