#include <RcppArmadillo.h>
#include <comat.h>
#include "na_prop.h"
#include "create_attributes.h"
// [[Rcpp::depends(comat)]]
using namespace Rcpp;

// https://stackoverflow.com/questions/31691130/conversion-of-r-matrices-to-armadillo-is-really-slow

// [[Rcpp::export]]
List get_polygons_cocoma(const arma::imat& x,
                         const arma::imat& y,
                         const arma::imat& m,
                         const arma::imat directions,
                         double threshold) {

  // get unique values of x and y
  List classes(2);

  classes(0) = comat::get_unique_values(wrap(x), true);
  classes(1) = comat::get_unique_values(wrap(y), true);


  arma::ivec classes_m = unique(m);
  classes_m = classes_m.elem(find(classes_m != NA_INTEGER));
  int classes_m_size = classes_m.size();

  // initialize objects
  List result(classes_m_size);
  arma::uvec ind_classes_m;
  arma::uvec ind_not_classes_m;
  arma::umat coords_classes_m;
  arma::imat submatrix_x;
  arma::imat submatrix_y;
  arma::imat submatrix_m;
  NumericVector na_perc(classes_m_size);

  // for each class in m
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

    if (na_perc(i) <= threshold){
      submatrix_y = y.submat(min_row, min_col, max_row, max_col);
      result[i] = comat::rcpp_get_cocoma_internal(wrap(submatrix_x), wrap(submatrix_y), directions, classes(0), classes(1));
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
  my_class(1) = "cocoma";
  df.attr("class") = my_class;
  return df;
}


/***R
library(raster)
a = as.matrix(raster("inst/raster/landcover.tif"))
b = as.matrix(raster("inst/raster/landform.tif"))
m = as.matrix(raster("inst/raster/ecoregions.tif"))
u = get_polygons_coma(a, m, directions = matrix(4), threshold = 0.5)
u
u2 = get_polygons_cocoma(a, b, m, directions = matrix(4), threshold = 0.5)
tibble::as_tibble(u2)
*/
