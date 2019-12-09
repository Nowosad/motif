#include <RcppArmadillo.h>
#include <comat.h>
// [[Rcpp::depends(comat)]]
#include "create_attributes.h"
#include "get_motifel_size.h"
#include "na_prop.h"
using namespace Rcpp;

// [[Rcpp::export]]
List get_polygons_fun(const List input,
                      const arma::imat& m,
                      Function f,
                      double threshold,
                      List classes) {

  int num_l = input.length();
  // List classes(num_l);
  // for (int l = 0; l < num_l; l++){
  //   classes(l) = comat::get_unique_values(input[l], true);
  // }

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
  NumericVector na_perc_all(num_l);
  List polygons_list(num_l);

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

    // create a submatrix of m
    submatrix_m = m.submat(min_row, min_col, max_row, max_col);

    // get ind of other classes in a submatrix of y
    ind_not_classes_m = find(submatrix_m != classes_m[i]);

    // replace other values by NA
    arma::ivec replacer(ind_not_classes_m.size());
    replacer.fill(NA_INTEGER);

    arma::imat one_layer = submatrix_m;

    for (int l = 0; l < num_l; l++){
      arma::imat layer_l = input(l);
      one_layer = layer_l.submat(min_row, min_col, max_row, max_col);
      one_layer.elem(ind_not_classes_m) = replacer;

      polygons_list(l) = one_layer;
      na_perc_all(l) = na_prop_polygon(one_layer, ind_not_classes_m.size());
    }

    na_perc(i) = mean(na_perc_all);

    if (na_perc(i) <= threshold){
      result[i] = f(polygons_list);
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
  my_class(1) = "fun";
  df.attr("class") = my_class;
  return df;
}

/***R
library(comat)
library(raster)
x = raster(system.file("raster/landcover.tif", package = "motif"))
# plot(landcover)
system.time({com2 = get_motifels_fun(as.matrix(x), f = fun2, size = 100, shift = 100, threshold = 0.9)})

sum2 = function(x, na.rm = TRUE) sum(x, na.rm = TRUE)

bench::mark(
  com1 = get_motifels_fun(as.matrix(x), f = sum, size = 100, shift = 100, threshold = 0.9),
  com2 = get_motifels_fun(as.matrix(x), f = sum2, size = 100, shift = 100, threshold = 0.9),
  check = FALSE
)

lsm_l_ent2 = function(x) landscapemetrics::lsm_l_ent(list(x))
com3 = get_motifels_fun(as.matrix(x), f = lsm_l_ent2, size = 100, shift = 100, threshold = 0.9)
*/
