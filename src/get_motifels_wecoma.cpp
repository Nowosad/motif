#include "get_motifels_wecoma.h"

// [[Rcpp::export]]
List get_motifels_wecoma(IntegerMatrix x,
                         NumericMatrix w,
                         const arma::imat directions,
                         int size,
                         int shift,
                         double threshold,
                         List classes,
                         const std::string fun,
                         const std::string na_action) {

  // List classes(1);
  // classes(0) = comat::get_unique_values(x, true);

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
  NumericVector na_perc(nr_of_motifels);

  int nr_of_motifels2 = 0;
  int m_row = 1;
  int m_col = 1;

  if (size == 0){
    all_nr_of_motifels(0) = 1;
    all_m_row(0) = m_row;
    all_m_col(0) = m_col;
    na_perc(0) = na_prop(x);
    if (na_perc(0) <= threshold){
      result[0] = comat::rcpp_get_wecoma_internal(x, w, directions, classes(0), fun, na_action);
    }
  } else {

    // IntegerMatrix motifel_x;
    // NumericMatrix motifel_w;

    for (int j = 0; j < num_c; j = j + shift){
      for (int i = 0; i < num_r; i = i + shift){
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
        na_perc(nr_of_motifels2) = na_prop(motifel_x);

        if (na_perc(nr_of_motifels2) <= threshold){
          NumericMatrix motifel_w = w(Range(i, i_max), Range(j, j_max));
          result[nr_of_motifels2] = comat::rcpp_get_wecoma_internal(motifel_x, motifel_w, directions, classes(0), fun, na_action);
        }

        nr_of_motifels2 ++;
        m_row++;
      }
      m_row = 1;
      m_col++;
    }

  }

  LogicalVector na_perc_below_thres = na_perc <= threshold;
  List df = List::create(Named("id") = all_nr_of_motifels[na_perc_below_thres],
                         Named("row") = all_m_row[na_perc_below_thres],
                         Named("col") = all_m_col[na_perc_below_thres],
                         Named("na_prop") = na_perc[na_perc_below_thres],
                         Named("signature") = result[na_perc_below_thres]);

  List attr = create_attributes(classes);
  df.attr("metadata") = attr;

  CharacterVector my_class(2);
  my_class(0) = "list";
  my_class(1) = "wecoma";
  df.attr("class") = my_class;
  return df;
}
