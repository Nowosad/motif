#include <RcppArmadillo.h>
#include <comat.h>
#include "na_prop.h"
// [[Rcpp::depends(comat)]]
using namespace Rcpp;

// https://stackoverflow.com/questions/31691130/conversion-of-r-matrices-to-armadillo-is-really-slow

// [[Rcpp::export]]
List get_polygons_coma(const arma::imat& x,
                             const arma::imat& y,
                             const arma::imat directions,
                             double threshold) {

  // get unique values of x and y
  std::vector<int> classes_x = comat::get_unique_values(wrap(x), true);

  arma::imat classes_y = unique(y);
  classes_y = classes_y.elem(find(classes_y != NA_INTEGER));
  int classes_y_size = classes_y.size();

  // initialize objects
  List result(classes_y_size);
  arma::uvec ind_classes_y;
  arma::uvec ind_not_classes_y;
  arma::umat coords_classes_y;
  arma::imat submatrix_x;
  arma::imat submatrix_y;
  double na_perc;

  // for each class in y
  for (int i = 0; i < classes_y_size; i++){

    // get class ind
    ind_classes_y = find(y == classes_y[i]);

    // convert class ind to coords (x, y)
    coords_classes_y = arma::ind2sub(size(y), ind_classes_y);
    arma::ucolvec min_coords = arma::min(coords_classes_y, 1);
    arma::ucolvec max_coords = arma::max(coords_classes_y, 1);

    // extract bounding box coords
    unsigned int min_row = min_coords(0);
    unsigned int max_row = max_coords(0);
    unsigned int min_col = min_coords(1);
    unsigned int max_col = max_coords(1);

    // create a submatrix of x
    submatrix_x = x.submat(min_row, min_col, max_row, max_col);
    // na_perc = na_prop(wrap(submatrix_x));
    na_perc = na_prop2(submatrix_x);
    // std::cout << "na prop:   " << na_perc << std::endl;

    // create a submatrix of y
    submatrix_y = y.submat(min_row, min_col, max_row, max_col);

    // get ind of other classes in a submatrix of y
    ind_not_classes_y = find(submatrix_y != classes_y[i]);

    // replace other values by NA
    arma::ivec replacer(ind_not_classes_y.size());
    replacer.fill(NA_INTEGER);
    submatrix_x.elem(ind_not_classes_y) = replacer;

    // wrap used here is very memory costly
    // try to think how it can be fixed
    // IntegerMatrix p = wrap(submatrix_x);
    // calculate coma
    // if (na_perc(0) <= threshold){
      result[i] = comat::rcpp_get_coma_internal(wrap(submatrix_x), directions, classes_x);
    // }

  }
  return result;
}

// [[Rcpp::export]]
List get_polygons_coma2(IntegerMatrix x,
                       const arma::imat& y,
                       const arma::imat directions,
                       double threshold) {

  // get unique values of x and y
  std::vector<int> classes_x = comat::get_unique_values(x, true);

  arma::imat classes_y = unique(y);
  classes_y = classes_y.elem(find(classes_y != NA_INTEGER));
  int classes_y_size = classes_y.size();

  // initialize objects
  List result(classes_y_size);
  arma::uvec ind_classes_y;
  arma::uvec ind_not_classes_y;
  arma::umat coords_classes_y;
  arma::imat submatrix_y;
  IntegerMatrix submatrix_x2;
  double na_perc;

  // for each class in y
  for (int i = 0; i < classes_y_size; i++){

    // get class ind
    ind_classes_y = find(y == classes_y[i]);

    // convert class ind to coords (x, y)
    coords_classes_y = arma::ind2sub(size(y), ind_classes_y);
    arma::ucolvec min_coords = arma::min(coords_classes_y, 1);
    arma::ucolvec max_coords = arma::max(coords_classes_y, 1);

    // extract bounding box coords
    unsigned int min_row = min_coords(0);
    unsigned int max_row = max_coords(0);
    unsigned int min_col = min_coords(1);
    unsigned int max_col = max_coords(1);

    // create a submatrix of x
    // submatrix_x = x.submat(min_row, min_col, max_row, max_col);
    submatrix_x2 = x(Range(min_row, max_row), Range(min_col, max_col));
    na_perc = na_prop(submatrix_x2);
    // na_perc = na_prop2(submatrix_x);
    // std::cout << "na prop:   " << na_perc << std::endl;

    // create a submatrix of y
    submatrix_y = y.submat(min_row, min_col, max_row, max_col);

    // get ind of other classes in a submatrix of y
    ind_not_classes_y = find(submatrix_y != classes_y[i]);

    // replace other values by NA
    arma::ivec replacer(ind_not_classes_y.size());
    replacer.fill(NA_INTEGER);
    submatrix_x.elem(ind_not_classes_y) = replacer;

    // wrap used here is very memory costly
    // try to think how it can be fixed
    // IntegerMatrix p = wrap(submatrix_x);
    // calculate coma
    // if (na_perc(0) <= threshold){
    result[i] = comat::rcpp_get_coma_internal(submatrix_x2, directions, classes_x);
    // }

  }
  return result;
}

/***R
library(raster)
a = as.matrix(raster("inst/raster/landcover.tif"))
b = as.matrix(raster("inst/raster/ecoregions.tif"))
u = get_polygons_coma(a, b, directions = matrix(4), threshold = 0.5)
u
u2 = lopata:::get_motifels_coma(a, directions = matrix(4), size = 650, shift = 650, threshold = 0.5)
u2

bench::mark(get_polygons_coma(a, b, directions = matrix(4), threshold = 0.5),
            get_polygons_coma2(a, b, directions = matrix(4), threshold = 0.5),
            lopata:::get_motifels_coma(a, directions = matrix(4), size = 650, shift = 650, threshold = 0.5),
            check = FALSE)

bench::mark(get_polygons_coma(a, b, directions = matrix(4), threshold = 0.5),
            get_polygons_coma2(a, b, directions = matrix(4), threshold = 0.5))

set.seed(1111)
a = matrix(sample(c(NA, 1:3), size = 20, replace = TRUE), ncol = 4)
b = matrix(c(2, 2, 2, 2, 3, 2, 2, 2, 3, 4, NA, NA, 4, 4, 4, 4, 4, 4, 4, 4), ncol = 4)
u = get_polygons_coma(a, b, directions = matrix(4), threshold = 0.5)
u

bench::mark(get_polygons_coma(a, b, directions = matrix(4), threshold = 0.5))
lopata::lop_coma(raster::raster(u), threshold = 0.9)$matrix
*/
