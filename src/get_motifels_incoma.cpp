#include "get_motifels_incoma.h"

// [[Rcpp::export]]
List get_motifels_incoma(const List input,
                         const arma::imat directions,
                         int size,
                         int shift,
                         double threshold) {

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
  NumericVector na_perc_all(num_l);
  NumericVector na_perc(nr_of_motifels);

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
      na_perc_all(l) = na_prop(input[l]);
    }
    na_perc[0] = mean(na_perc_all);
    if (na_perc(0) <= threshold){
      result[0] = comat::rcpp_get_incoma_list(input, directions, classes);
      result[0] = comat::rcpp_get_incoma_matrix(result[0]);
    }

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
          na_perc_all(l) = na_prop(motifel_input[l]);
        }

        na_perc[nr_of_motifels2] = mean(na_perc_all);

        if (na_perc(nr_of_motifels2) <= threshold){
          result[nr_of_motifels2] = comat::rcpp_get_incoma_list(motifel_input, directions, classes);
          result[nr_of_motifels2] = comat::rcpp_get_incoma_matrix(result[nr_of_motifels2]);
        }

        nr_of_motifels2 ++;
        m_col++;
      }
      m_col = 1;
      m_row++;
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
  my_class(1) = "incoma";
  df.attr("class") = my_class;
  return df;
}

/***R
library(comat)
library(raster)
x = raster(system.file("raster/landcover.tif", package = "lopata"))
y = raster(system.file("raster/landform.tif", package = "lopata"))
# plot(landcover)
incom2 = get_motifels_incoma(list(as.matrix(x), as.matrix(y)),
                                             directions = matrix(4),
                                             size = 100, shift = 100,
                                             threshold = 1)
bench::mark(
  incom12 = get_motifels_incoma(list(as.matrix(x), as.matrix(y)), directions = matrix(4), size = 100, shift = 100, threshold = 0.1),
  incom22 = get_motifels_incoma(list(as.matrix(x), as.matrix(y)), directions = matrix(4), size = 100, shift = 100, threshold = 0.5),
  incom32 = get_motifels_incoma(list(as.matrix(x), as.matrix(y)), directions = matrix(4), size = 100, shift = 100, threshold = 1),
  check = FALSE
)
*/
