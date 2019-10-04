#include "get_motifels_coma.h"
using namespace Rcpp;

// [[Rcpp::export]]
List get_motifels_coma(IntegerMatrix x,
                       const arma::imat directions,
                       int size,
                       int shift) {

  int num_r = x.nrow();
  int num_c = x.ncol();

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

  int nr_of_motifels2 = 0;
  int m_row = 1;
  int m_col = 1;

  if (size == 0){
    all_nr_of_motifels(0) = 1;
    all_m_row(0) = m_row;
    all_m_col(0) = m_col;
    result[0] = comat::rcpp_get_coma_internal(x, directions, classes(0));
  } else {

    IntegerMatrix motifel_x;

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
        motifel_x = x(Range(i, i_max), Range(j, j_max));
        result[nr_of_motifels2] = comat::rcpp_get_coma_internal(motifel_x, directions, classes(0));

        // double na_perc = na_prop(motifel_x);

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
                         Named("matrix") = result);
  df.attr("metadata") = attr;

  CharacterVector my_class(2);
  my_class(0) = "list";
  my_class(1) = "coma";
  df.attr("class") = my_class;
  return df;
}
