#include "create_attributes.h"

// [[Rcpp::export]]
List create_attributes(List x) {
  int num_l = x.length();

  IntegerVector id(num_l);
  IntegerVector nr_unique_vals(num_l);
  List unique_vals(num_l);

  for (int i = 0; i < num_l; i++){
    IntegerVector unique_vals_i = x(i);
    unique_vals(i) = unique_vals_i;
    nr_unique_vals(i) = unique_vals_i.length();
    id(i) = i + 1;
  }

  List attr = List::create(Named("id") = id,
                           Named("no_unique") = nr_unique_vals,
                           Named("vals") = unique_vals);
  return attr;
}




/*** R
create_attributes
*/
