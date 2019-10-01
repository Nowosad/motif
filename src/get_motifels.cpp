#include <comat.h>
// [[Rcpp::depends(comat)]]
#include "create_attributes.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::interfaces(r, cpp)]]
using namespace Rcpp;

// [[Rcpp::export]]
int get_motifel_size(int num_r, int num_c, int shift){
  int nr_of_motifels = 0;
  for (int i = 0; i < num_r; i = i + shift) {
    for (int j = 0; j < num_c; j = j + shift) {
      nr_of_motifels ++;
    }
  }
  return nr_of_motifels;
}

// [[Rcpp::export]]
List get_motifels_coma(IntegerMatrix x,
                       const arma::imat directions,
                       int size,
                       int shift,
                       const std::string fun = "mean",
                       const std::string na_action = "replace") {

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

// [[Rcpp::export]]
List get_motifels_wecoma(IntegerMatrix x,
                         NumericMatrix w,
                         const arma::imat directions,
                         int size,
                         int shift,
                         const std::string fun = "mean",
                         const std::string na_action = "replace") {

  List classes(1);
  classes(0) = comat::get_unique_values(x, true);

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

  int nr_of_motifels2 = 0;
  int m_row = 1;
  int m_col = 1;

  if (size == 0){
    all_nr_of_motifels(0) = 1;
    all_m_row(0) = m_row;
    all_m_col(0) = m_col;
    result[0] = comat::rcpp_get_wecoma_internal(x, w, directions, classes(0), fun, na_action);
  } else {

    IntegerMatrix motifel_x;
    NumericMatrix motifel_w;

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
        motifel_w = w(Range(i, i_max), Range(j, j_max));
        result[nr_of_motifels2] = comat::rcpp_get_wecoma_internal(motifel_x, motifel_w, directions, classes(0), fun, na_action);
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
  my_class(1) = "wecoma";
  df.attr("class") = my_class;
  return df;
}


// [[Rcpp::export]]
List get_motifels_cocoma(IntegerMatrix x,
                         IntegerMatrix y,
                         const arma::imat directions,
                         int size,
                         int shift) {

  List classes(2);

  classes(0) = comat::get_unique_values(x, true);
  classes(1) = comat::get_unique_values(y, true);
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

  int nr_of_motifels2 = 0;
  int m_row = 1;
  int m_col = 1;

  if (size == 0){
    all_nr_of_motifels(0) = 1;
    all_m_row(0) = m_row;
    all_m_col(0) = m_col;
    result[0] = comat::rcpp_get_cocoma_internal(x, y, directions, classes(0), classes(1));
  } else {

    IntegerMatrix motifel_x;
    IntegerMatrix motifel_y;

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
        motifel_y = y(Range(i, i_max), Range(j, j_max));
        result[nr_of_motifels2] = comat::rcpp_get_cocoma_internal(motifel_x, motifel_y, directions, classes(0), classes(1));

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
  my_class(1) = "cocoma";
  df.attr("class") = my_class;
  return df;
}



// [[Rcpp::export]]
List get_motifels_incoma(const List input,
                  const arma::imat directions,
                  int size,
                  int shift,
                  const std::string fun = "mean",
                  const std::string na_action = "replace") {

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
    result[0] = comat::rcpp_get_incoma_internal(input, directions, classes);

  } else {
    List motifel_input(num_l);
    IntegerMatrix layer_l(num_r, num_c);

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

          layer_l = wrap(input(l));
          motifel_input(l) = layer_l(Range(i, i_max), Range(j, j_max));
        }
        result[nr_of_motifels2] = comat::rcpp_get_incoma_internal(motifel_input, directions, classes);

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
  my_class(1) = "incoma";
  df.attr("class") = my_class;
  return df;
}

/*** R
library(raster)
set.seed(10100)
l1 = raster(matrix(sample(1:2, size = 1000000, replace = TRUE), ncol = 1000))
l2 = raster(matrix(sample(c(9, 6, 3), size = 1000000, replace = TRUE), ncol = 1000))
x = stack(l1, l2, l1)
rasters = lapply(raster::as.list(x), raster::as.matrix)
directions = as.matrix(4)
a = list(as.matrix(l1))
res = get_motifels(a, directions = directions, size = 100, shift = 100)
library(tibble)
res_df = as_tibble(res)

get_cove(res_df$V3[[4]])
*/
