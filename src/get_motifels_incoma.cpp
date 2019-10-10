#include "get_motifels_incoma.h"

// [[Rcpp::export]]
List get_motifels_incoma(const List input,
                         const arma::imat directions,
                         int size,
                         int shift) {

  int num_l = input.length();
  List classes(num_l);

  IntegerMatrix x = input(0);
  int num_r = x.nrow();
  int num_c = x.ncol();

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
  NumericVector na_perc(num_l);

  int nr_of_motifels2 = 0;
  int m_row = 1;
  int m_col = 1;

  for (int l = 0; l < num_l; l++){
    classes(l) = comat::get_unique_values(input[l], true);
  }

  if (size == 0){
    all_nr_of_motifels(0) = 1;
    all_m_row(0) = m_row;
    all_m_col(0) = m_col;
    for (int l = 0; l < num_l; l++){
      na_perc(l) = na_prop(input[l]);
    }
    result[0] = comat::rcpp_get_incoma_list(input, directions, classes);
    result[0] = comat::rcpp_get_incoma_matrix(result[0]);

  } else {
    List motifel_input(num_l);
    // IntegerMatrix layer_l(num_r, num_c);

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

        for (int l = 0; l < num_l; l++){
          // IntegerMatrix layer_l;

          IntegerMatrix layer_l = wrap(input(l));
          motifel_input(l) = layer_l(Range(i, i_max), Range(j, j_max));
        }
        result[nr_of_motifels2] = comat::rcpp_get_incoma_list(motifel_input, directions, classes);
        result[nr_of_motifels2] = comat::rcpp_get_incoma_matrix(result[nr_of_motifels2]);
        // double na_perc = na_prop(motifel_x);

        nr_of_motifels2 ++;
        m_col++;
      }
      m_col = 1;
      m_row++;
    }
  }

  double na_perc_mean = mean(na_perc);

  List attr = create_attributes(classes);

  List df = List::create(Named("id") = all_nr_of_motifels,
                         Named("row") = all_m_row,
                         Named("col") = all_m_col,
                         Named("na_prop") = na_perc_mean,
                         Named("matrix") = result);
  df.attr("metadata") = attr;

  CharacterVector my_class(2);
  my_class(0) = "list";
  my_class(1) = "incoma";
  df.attr("class") = my_class;
  return df;
}
