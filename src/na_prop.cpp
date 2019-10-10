#include "na_prop.h"

// calculates a proportion of cells with NA's
double na_prop(IntegerMatrix x) {

  double na_of_cells = x.length();
  double na_of_na = std::count_if(x.begin(), x.end(),
                               [](double x){return x == NA_INTEGER;});

  return na_of_na / na_of_cells;
}

/*** R
a = matrix(c(1:9), ncol = 3)
b = matrix(c(1:5, rep(NA, 4)), ncol = 3)
d = matrix(c(1:4, rep(NA, 5)), ncol = 3)

na_prop(a)
na_prop(b)
na_prop(d)
*/
