#include "get_motifels_coma.h"
using namespace Rcpp;

// [[Rcpp::export]]
List get_motifels_coma(IntegerMatrix x,
                       const arma::imat directions,
                       int size,
                       int shift) {

  const int num_r = x.nrow();
  const int num_c = x.ncol();

  List classes(1);
  classes(0) = comat::get_unique_values(x, true);

  int nr_of_motifels;
  if (size == 0){
    nr_of_motifels = 1;
  } else {
    nr_of_motifels = get_motifel_size(num_r, num_c, shift);
  }

  List result(nr_of_motifels);
  IntegerVector all_nr_of_motifels(nr_of_motifels);
  IntegerVector all_m_row(nr_of_motifels);
  IntegerVector all_m_col(nr_of_motifels);
  NumericVector na_perc(nr_of_motifels);


  int nr_of_motifels2 = 0;
  int m_row = 1;
  int m_col = 1;

  if (size == 0){
    all_nr_of_motifels(0) = 1;
    all_m_row(0) = m_row;
    all_m_col(0) = m_col;
    na_perc(0) = na_prop(x);
    result[0] = comat::rcpp_get_coma_internal(x, directions, classes(0));
  } else {

    // IntegerMatrix motifel_x;

    for (int i = 0; i < num_r; i = i + shift){
      for (int j = 0; j < num_c; j = j + shift){
        all_nr_of_motifels(nr_of_motifels2) = nr_of_motifels2 + 1;
        all_m_row(nr_of_motifels2) = m_row;
        all_m_col(nr_of_motifels2) = m_col;

        int i_max = i + (size - 1);
        if (i_max >= num_r){
          i_max = num_r - 1;
        }
        int j_max = j + (size - 1);
        if (j_max >= num_c){
          j_max = num_c - 1;
        }
        IntegerMatrix motifel_x = x(Range(i, i_max), Range(j, j_max));
        // Rcout << "The value of motifel_x : " << motifel_x << "\n";

        result[nr_of_motifels2] = comat::rcpp_get_coma_internal(motifel_x, directions, classes(0));

        na_perc(nr_of_motifels2) = na_prop(motifel_x);

        nr_of_motifels2 ++;
        m_col++;
      }
      m_col = 1;
      m_row++;
    }
  }

  List attr = create_attributes(classes);

  List df = List::create(Named("id") = all_nr_of_motifels,
                         Named("row") = all_m_row,
                         Named("col") = all_m_col,
                         Named("na_prop") = na_perc,
                         Named("matrix") = result);
  df.attr("metadata") = attr;

  CharacterVector my_class(2);
  my_class(0) = "list";
  my_class(1) = "coma";
  df.attr("class") = my_class;
  return df;
}


/***R
data(x, package = "comat")
x = raster::as.matrix(x)
# com2 = get_motifels_coma(x, directions = matrix(4), size = 0, shift = 0)
com2 = get_motifels_coma(x, directions = matrix(4), size = 2, shift = 2)
get_motifels_coma3(x, directions = matrix(4), size = 2, shift = 2)

x = matrix(sample(1:2, size = 1000000, replace = TRUE), ncol = 1000)

bench::mark(get_motifels_coma(x, directions = matrix(4), size = 100, shift = 100))
*/
