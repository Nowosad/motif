#include "get_unique_values.h"
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<int> get_unique_values(const Rcpp::IntegerVector &x, bool na_omit)
{
  std::set<int> s;
  unsigned size = x.size();
  for(unsigned i = 0; i < size; i++) {
    s.insert(x[i]);
  }

  std::vector<int> classes(s.begin(), s.end());

  if (na_omit) {
    const int na = NA_INTEGER;
    if (classes[0] == na)
      classes.erase(classes.begin() + 0);
  }
  return classes;
}


/*** R
x = sample(1:5, size = 1e7, replace = TRUE)

bench::mark(
  get_unique_values(x, TRUE)
)
*/
