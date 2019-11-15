#include "na_prop.h"

// calculates a proportion of cells with NA's
double na_prop(const IntegerMatrix& x) {

  double no_of_cells = x.length();
  double no_of_na = std::count_if(x.begin(), x.end(),
                               [](double x){return x == NA_INTEGER;});

  return no_of_na / no_of_cells;
}

// calculates a proportion of cells with NA's
double na_prop_polygon(const arma::imat& x, const double& no_of_outside_cells) {

  double na_of_cells = x.size() - no_of_outside_cells;
  double na_of_na = std::count_if(x.begin(), x.end(), [](double x){return x == NA_INTEGER;}) - no_of_outside_cells;

  return na_of_na / na_of_cells;
}


/*** R
a = matrix(c(1:9), ncol = 3)
b = matrix(c(1:5, rep(NA, 4)), ncol = 3)
d = matrix(c(1:4, rep(NA, 5)), ncol = 3)

na_prop(a)
na_prop(b)
na_prop(d)
na_prop2(d)
*/
