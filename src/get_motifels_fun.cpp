#include <RcppArmadillo.h>
#include <comat.h>
// [[Rcpp::depends(comat)]]
#include "create_attributes.h"
#include "get_motifel_size.h"
#include "na_prop.h"
using namespace Rcpp;

// [[Rcpp::export]]
List get_motifels_fun(IntegerMatrix x,
                      Function f,
                      int size,
                      int shift,
                      double threshold) {

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
    if (na_perc(0) <= threshold){
      result[0] = f(x);
    }
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

        na_perc(nr_of_motifels2) = na_prop(motifel_x);
        if (na_perc(nr_of_motifels2) <= threshold){
          result[nr_of_motifels2] = f(motifel_x);
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
                         Named("matrix") = result[na_perc_below_thres]);

  List attr = create_attributes(classes);
  df.attr("metadata") = attr;

  CharacterVector my_class(2);
  my_class(0) = "list";
  my_class(1) = "fun";
  df.attr("class") = my_class;
  return df;
}

/***R
library(comat)
library(raster)
x = raster(system.file("raster/landcover.tif", package = "lopata"))
# plot(landcover)
system.time({com2 = get_motifels_fun(as.matrix(x), f = fun2, size = 100, shift = 100, threshold = 0.9)})

sum2 = function(x, na.rm = TRUE) sum(x, na.rm = TRUE)

bench::mark(
  com1 = get_motifels_fun(as.matrix(x), f = sum, size = 100, shift = 100, threshold = 0.9),
  com2 = get_motifels_fun(as.matrix(x), f = sum2, size = 100, shift = 100, threshold = 0.9),
  check = FALSE
)

lsm_l_ent2 = function(x) landscapemetrics::lsm_l_ent(list(x))
com3 = get_motifels_fun(as.matrix(x), f = lsm_l_ent2, size = 100, shift = 100, threshold = 0.9)
*/
