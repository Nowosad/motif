#include <RcppArmadillo.h>
#include <comat.h>
#include "na_prop.h"
#include "create_attributes.h"
#include "get_composition.h"
// [[Rcpp::depends(comat)]]
using namespace Rcpp;

// [[Rcpp::export]]
List get_polygons_composition(const arma::imat& x,
                       const arma::imat& m,
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
      result[i] = get_composition(wrap(submatrix_x), classes(0));
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
*/
