#include <RcppArmadillo.h>
#include <comat.h>
#include "na_prop.h"
#include "create_attributes.h"
// [[Rcpp::depends(comat)]]
using namespace Rcpp;

// https://stackoverflow.com/questions/31691130/conversion-of-r-matrices-to-armadillo-is-really-slow

// [[Rcpp::export]]
List get_polygons_coma(const arma::imat& x,
                       const arma::imat& m,
                       const arma::imat directions,
                       double threshold,
                       List classes) {

  // get unique values of x and y
  // List classes_x(1);
  // classes_x(0) = comat::get_unique_values(wrap(x), true);

  arma::ivec classes_m = unique(m);
  classes_m = classes_m.elem(find(classes_m != NA_INTEGER));
  int classes_m_size = classes_m.size();

  // initialize objects
  List result(classes_m_size);
  arma::uvec ind_classes_m;
  arma::uvec ind_not_classes_m;
  arma::umat coords_classes_m;
  arma::imat submatrix_x;
  arma::imat submatrix_m;
  NumericVector na_perc(classes_m_size);

  // for each class in y
  for (int i = 0; i < classes_m_size; i++){

    // get class ind
    ind_classes_m = find(m == classes_m[i]);

    // convert class ind to coords (x, y)
    coords_classes_m = arma::ind2sub(size(m), ind_classes_m);
    arma::ucolvec min_coords = arma::min(coords_classes_m, 1);
    arma::ucolvec max_coords = arma::max(coords_classes_m, 1);

    // extract bounding box coords
    unsigned int min_row = min_coords(0);
    unsigned int max_row = max_coords(0);
    unsigned int min_col = min_coords(1);
    unsigned int max_col = max_coords(1);

    // create a submatrix of x
    submatrix_x = x.submat(min_row, min_col, max_row, max_col);

    // create a submatrix of m
    submatrix_m = m.submat(min_row, min_col, max_row, max_col);

    // get ind of other classes in a submatrix of y
    ind_not_classes_m = find(submatrix_m != classes_m[i]);

    // replace other values by NA
    arma::ivec replacer(ind_not_classes_m.size());
    replacer.fill(NA_INTEGER);
    submatrix_x.elem(ind_not_classes_m) = replacer;

    // calculate share of NAs
    na_perc(i) = na_prop_polygon(submatrix_x, ind_not_classes_m.size());
    // std::cout << "na prop:   " << na_perc(i) << std::endl;

    // wrap used here is very memory costly
    // try to think how it can be fixed
    // IntegerMatrix p = wrap(submatrix_x);
    // calculate coma
    if (na_perc(i) <= threshold){
      result[i] = comat::rcpp_get_coma_internal(wrap(submatrix_x), directions, classes(0));
    }
  }
  LogicalVector na_perc_below_thres = na_perc <= threshold;
  IntegerVector ids = (wrap(classes_m));
  List df = List::create(Named("id") = ids[na_perc_below_thres],
                         Named("na_prop") = na_perc[na_perc_below_thres],
                         Named("signature") = result[na_perc_below_thres]);

  List attr = create_attributes(classes);
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
u2 = motif:::get_motifels_coma(a, directions = matrix(4), size = 650, shift = 650, threshold = 0.5)
u2

bench::mark(motif:::get_polygons_coma(a, b, directions = matrix(4), threshold = 0.5),
            motif:::get_motifels_coma(a, directions = matrix(4), size = 650, shift = 650, threshold = 0.5),
            check = FALSE)


tibble::as_tibble(u)
# n = structure(n, class = c("coma", class(n)))
n2 = motif::lop_cove(n)
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
motif::lop_coma(raster::raster(u), threshold = 0.9)$matrix
*/
