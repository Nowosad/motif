#include <RcppArmadillo.h>
#include <comat.h>
#include "na_prop.h"
#include "create_attributes.h"
// [[Rcpp::depends(comat)]]
using namespace Rcpp;

// https://stackoverflow.com/questions/31691130/conversion-of-r-matrices-to-armadillo-is-really-slow

// [[Rcpp::export]]
List get_polygons_coma(const arma::imat& x,
                       const arma::imat& y,
                       const arma::imat directions,
                       double threshold) {

  // get unique values of x and y
  List classes_x(1);
  classes_x(0) = comat::get_unique_values(wrap(x), true);

  arma::ivec classes_y = unique(y);
  classes_y = classes_y.elem(find(classes_y != NA_INTEGER));
  int classes_y_size = classes_y.size();

  // initialize objects
  List result(classes_y_size);
  arma::uvec ind_classes_y;
  arma::uvec ind_not_classes_y;
  arma::umat coords_classes_y;
  arma::imat submatrix_x;
  arma::imat submatrix_y;
  NumericVector na_perc(classes_y_size);

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

    // create a submatrix of y
    submatrix_y = y.submat(min_row, min_col, max_row, max_col);

    // get ind of other classes in a submatrix of y
    ind_not_classes_y = find(submatrix_y != classes_y[i]);

    // replace other values by NA
    arma::ivec replacer(ind_not_classes_y.size());
    replacer.fill(NA_INTEGER);
    submatrix_x.elem(ind_not_classes_y) = replacer;

    // calculate share of NAs
    na_perc(i) = na_prop_polygon(submatrix_x, ind_not_classes_y.size());
    // std::cout << "na prop:   " << na_perc(i) << std::endl;

    // wrap used here is very memory costly
    // try to think how it can be fixed
    // IntegerMatrix p = wrap(submatrix_x);
    // calculate coma
    result[i] = comat::rcpp_get_coma_internal(wrap(submatrix_x), directions, classes_x(0));

  }
  // IntegerVector classes_y_rcpp = wrap(classes_y);
  List df = List::create(Named("id") = as<std::vector<int> >(wrap(classes_y)),
                         Named("na_prop") = na_perc,
                         Named("matrix") = result);

  List attr = create_attributes(classes_x);
  df.attr("metadata") = attr;

  CharacterVector my_class(2);
  my_class(0) = "list";
  my_class(1) = "coma";
  df.attr("class") = my_class;
  return df;
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
            lopata:::get_motifels_coma(a, directions = matrix(4), size = 650, shift = 650, threshold = 0.5),
            check = FALSE)


# n = tibble::as_tibble(u)
# n = structure(n, class = c("coma", class(n)))
n2 = lopata::lop_cove(n)
# library(sf)
# eco = read_sf("inst/vector/ecoregions.gpkg")
# new = dplyr::bind_cols(eco, n)
# write_sf(new, "test.gpkg")

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
