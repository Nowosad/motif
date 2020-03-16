#include "get_composition.h"

std::map<int, unsigned> get_class_index_map(const std::vector<int> &classes)
{
  std::map<int, unsigned> class_index;
  for (unsigned i = 0; i < classes.size(); i++) {
    class_index.insert(std::make_pair(classes[i], i));
  }
  return class_index;
}

// [[Rcpp::export]]
IntegerVector get_composition(const IntegerMatrix& x, std::vector<int> classes){

  //std::vector<int> classes = get_unique_values(x, true);

  const std::map<int, unsigned> class_index = get_class_index_map(classes);

  IntegerVector v(classes.size());

  for (int i = 0; i < x.size(); i++){
    int tmp = x[i];
    if (class_index.count(tmp) == 0)
      continue;
    unsigned focal_class = class_index.at(tmp);
    v[focal_class]++;
  }

  // IntegerVector v = table(na_omit(x));
  v.attr("dim") = Dimension(1, v.size());
  colnames(v) = wrap(classes);

  return v;
}

// //[[Rcpp::export]]
// IntegerVector get_composition_list(List x, std::vector<int> classes){
//   if (x.size() > 1){
//     warning("Composition is calculated for the first layer only");
//   }
//   IntegerMatrix m = x(0);
//   return get_composition(m, classes);
// }
//
// // [[Rcpp::export]]
// IntegerVector rcpp_get_composition_vector(const Rcpp::NumericVector & x)
// {
//   return table(na_omit(x));
// }

/*** R
library(raster)
new_r = raster(nrows = 3, ncols = 3, vals = c(rep(1, 7), NA, 3))
plot(new_r)
new_m = as.matrix(new_r)
classes = motif:::get_unique_values(new_m, TRUE)
aaa = get_composition(new_m, classes)
aaa
get_composition_list(list(new_m), classes)
# rcpp_get_composition_vector(new_m)
*/
