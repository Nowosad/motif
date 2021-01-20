#include "get_motifels.h"

// [[Rcpp::export]]
List get_motifels(const List& input,
                  std::string type,
                  const arma::imat directions,
                  int size,
                  int shift,
                  Function f,
                  double threshold,
                  List classes,
                  const std::string fun,
                  const std::string na_action){

  int num_l = input.length();

  IntegerMatrix x = input(0);
  const int num_r = x.nrow();
  const int num_c = x.ncol();

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

  if (size == 0){
    all_nr_of_motifels(0) = 1;
    all_m_row(0) = m_row;
    all_m_col(0) = m_col;
    if (type == "coma"){
      na_perc(0) = na_prop(x, size);
      if (na_perc(0) <= threshold){
        result[0] = comat::rcpp_get_coma_internal(x, directions, classes(0));
      }
    } else if (type == "cocoma"){
      IntegerMatrix y = input(1);
      na_perc(0) = na_prop(x, size);
      if (na_perc(0) <= threshold){
        result[0] = comat::rcpp_get_cocoma_internal(x, y, directions, classes(0), classes(1));
      }
    } else if (type == "wecoma"){
      NumericMatrix w = input(1);
      na_perc(0) = na_prop(x, size);
      if (na_perc(0) <= threshold){
        result[0] = comat::rcpp_get_wecoma_internal(x, w, directions, classes(0), fun, na_action);
      }
    } else if (type == "incoma"){
      for (int l = 0; l < num_l; l++){
        na_perc_all(l) = na_prop(input[l], size);
      }
      na_perc[0] = max(na_perc_all);
      if (na_perc(0) <= threshold){
        result[0] = comat::rcpp_get_incoma_list(input, directions, classes);
        result[0] = comat::rcpp_get_incoma_matrix(result[0]);
      }
    } else if (type == "composition"){
      na_perc(0) = na_prop(x, size);
      if (na_perc(0) <= threshold){
        result[0] = get_composition(x, classes(0));
      }
    } else if (type == "fun"){
      for (int l = 0; l < num_l; l++){
        na_perc_all(l) = na_prop(input[l], size);
      }
      na_perc[0] = max(na_perc_all);
      if (na_perc(0) <= threshold){
        result[0] = f(input);
      }
    }
  } else {
    List motifel_input(num_l);
    // IntegerMatrix layer_l(num_r, num_c);
    // if (type == "cocoma"){
    //   IntegerMatrix y = input(1);
    // } else if (type == "wecoma"){
    //   NumericMatrix w = input(1);
    // }

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

        if (type == "coma"){
          IntegerMatrix motifel_x = x(Range(i, i_max), Range(j, j_max));
          // Rcout << "The value of motifel_x : " << motifel_x << "\n";

          na_perc(nr_of_motifels2) = na_prop(motifel_x, size);
          if (na_perc(nr_of_motifels2) <= threshold){
            result[nr_of_motifels2] = comat::rcpp_get_coma_internal(motifel_x, directions, classes(0));
          }

        } else if (type == "cocoma"){
          IntegerMatrix y = input(1);
          IntegerMatrix motifel_x = x(Range(i, i_max), Range(j, j_max));
          na_perc(nr_of_motifels2) = na_prop(motifel_x, size);
          if (na_perc(nr_of_motifels2) <= threshold){
            IntegerMatrix motifel_y = y(Range(i, i_max), Range(j, j_max));
            result[nr_of_motifels2] = comat::rcpp_get_cocoma_internal(motifel_x, motifel_y, directions, classes(0), classes(1));
          }
        } else if (type == "wecoma"){
          NumericMatrix w = input(1);
          IntegerMatrix motifel_x = x(Range(i, i_max), Range(j, j_max));
          na_perc(nr_of_motifels2) = na_prop(motifel_x, size);
          if (na_perc(nr_of_motifels2) <= threshold){
            NumericMatrix motifel_w = w(Range(i, i_max), Range(j, j_max));
            result[nr_of_motifels2] = comat::rcpp_get_wecoma_internal(motifel_x, motifel_w, directions, classes(0), fun, na_action);
          }
        } else if (type == "incoma"){
          for (int l = 0; l < num_l; l++){
            // IntegerMatrix layer_l;
            IntegerMatrix layer_l = wrap(input(l));
            motifel_input(l) = layer_l(Range(i, i_max), Range(j, j_max));
            na_perc_all(l) = na_prop(motifel_input[l], size);
          }

          na_perc[nr_of_motifels2] = max(na_perc_all);

          if (na_perc(nr_of_motifels2) <= threshold){
            result[nr_of_motifels2] = comat::rcpp_get_incoma_list(motifel_input, directions, classes);
            result[nr_of_motifels2] = comat::rcpp_get_incoma_matrix(result[nr_of_motifels2]);
          }
        } else if (type == "composition"){
          IntegerMatrix motifel_x = x(Range(i, i_max), Range(j, j_max));

          na_perc(nr_of_motifels2) = na_prop(motifel_x, size);
          if (na_perc(nr_of_motifels2) <= threshold){
            result[nr_of_motifels2] = get_composition(motifel_x, classes(0));
          }
        } else if (type == "fun"){
          for (int l = 0; l < num_l; l++){
            // IntegerMatrix layer_l;
            IntegerMatrix layer_l = wrap(input(l));
            motifel_input(l) = layer_l(Range(i, i_max), Range(j, j_max));
            na_perc_all(l) = na_prop(motifel_input[l], size);
          }

          na_perc[nr_of_motifels2] = max(na_perc_all);

          if (na_perc(nr_of_motifels2) <= threshold){
            result[nr_of_motifels2] = f(motifel_input);
          }
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
                         Named("na_prop") = na_perc[na_perc_below_thres],
                                                   Named("signature") = result[na_perc_below_thres]);

  List attr = create_attributes(classes);
  df.attr("metadata") = attr;

  CharacterVector my_class(2);
  my_class(0) = "list";
  my_class(1) = type;
  df.attr("class") = my_class;
  return df;
}

/*** R
library(stars)
my_dir = matrix(4)
x = read_stars(system.file("raster/landcover2015.tif", package = "motif"))[[1]]
y = read_stars(system.file("raster/landform.tif", package = "motif"))[[1]]
mode(y) = "integer"
my_classes_x = motif:::get_unique_values(x, TRUE)
my_classes_y = motif:::get_unique_values(y, TRUE)
my_classes = list(my_classes_x, my_classes_y)
my_fun = function(x) mean(x[[1]], na.rm = TRUE)

# coma
coma_bench = bench::mark(
  {coma1 = motif:::get_motifels_coma(x, directions = my_dir,
                                    size = 0, shift = 0,
                                    threshold = 0.5, classes = my_classes[1])},
  {coma2 = get_motifels(list(x), type = "coma", directions = my_dir,
                       size = 0, shift = 0, f = function(){},
                       threshold = 0.5, classes = my_classes[1],
                       fun = NA_character_, na_action = NA_character_)}
)
coma_bench[2:5]

#cocoma
cocoma_bench = bench::mark(
  {cocoma1 = motif:::get_motifels_cocoma(x, y, directions = my_dir,
                                     size = 0, shift = 0,
                                     threshold = 0.5, classes = my_classes)},
  {cocoma2 = get_motifels(list(x, y), type = "cocoma", directions = my_dir,
                        size = 0, shift = 0, f = function(){},
                        threshold = 0.5, classes = my_classes,
                        fun = NA_character_, na_action = NA_character_)}
)
cocoma_bench[2:5]

#wecoma
wecoma_bench = bench::mark(
  {wecoma1 = motif:::get_motifels_wecoma(x, y, directions = my_dir,
                                         size = 0, shift = 0,
                                         threshold = 0.5, classes = my_classes,
                                         fun = "mean", na_action = "replace")},
  {wecoma2 = get_motifels(list(x, y), type = "wecoma", directions = my_dir,
                          size = 0, shift = 0, f = function(){},
                          threshold = 0.5, classes = my_classes,
                          fun = "mean", na_action = "replace")}
)
wecoma_bench[2:5]

#incoma
incoma_bench = bench::mark(
  {incoma1 = motif:::get_motifels_incoma(list(x, y), directions = my_dir,
                                         size = 0, shift = 0,
                                         threshold = 0.5, classes = my_classes)},
  {incoma2 = get_motifels(list(x, y), type = "incoma", directions = my_dir,
                          size = 0, shift = 0, f = function(){},
                          threshold = 0.5, classes = my_classes,
                          fun = NA_character_, na_action = NA_character_)}
)
incoma_bench[2:5]

#composition
composition_bench = bench::mark(
  {composition1 = motif:::get_motifels_composition(x,
                                     size = 0, shift = 0,
                                     threshold = 0.5, classes = my_classes[1])},
  {composition2 = get_motifels(list(x), type = "composition", directions = matrix(NA),
                        size = 0, shift = 0, f = function(){},
                        threshold = 0.5, classes = my_classes[1],
                        fun = NA_character_, na_action = NA_character_)}
)
composition_bench[2:5]

#fun
fun_bench = bench::mark(
  {fun1 = motif:::get_motifels_fun(list(x),
                                   size = 0, shift = 0,
                                   f = my_fun,
                                   threshold = 0.5, classes = my_classes[1])},
  {fun2 = get_motifels(list(x), type = "fun", directions = matrix(NA),
                       size = 0, shift = 0,
                       f = my_fun,
                       threshold = 0.5, classes = my_classes[1],
                       fun = NA_character_, na_action = NA_character_)}
)
fun_bench[2:5]

#coma
coma_bench2 = bench::mark(
  {coma21 = motif:::get_motifels_coma(x, directions = my_dir,
                                     size = 100, shift = 100,
                                     threshold = 0.5, classes = my_classes[1])},
  {coma22 = get_motifels(list(x), type = "coma", directions = my_dir,
                        size = 100, shift = 100, f = function(){},
                        threshold = 0.5, classes = my_classes[1],
                        fun = NA_character_, na_action = NA_character_)}
)
coma_bench2[2:5]

#cocoma
cocoma_bench2 = bench::mark(
  {cocoma21 = motif:::get_motifels_cocoma(x, y, directions = my_dir,
                                         size = 500, shift = 500,
                                         threshold = 0.5, classes = my_classes)},
  {cocoma22 = get_motifels(list(x, y), type = "cocoma", directions = my_dir,
                           size = 500, shift = 500, f = function(){},
                           threshold = 0.5, classes = my_classes,
                           fun = NA_character_, na_action = NA_character_)},
  iterations = 1
)
cocoma_bench2[2:5]

#wecoma2
wecoma_bench2 = bench::mark(
  {wecoma21 = motif:::get_motifels_wecoma(x, y, directions = my_dir,
                                         size = 100, shift = 100,
                                         threshold = 0.5, classes = my_classes,
                                         fun = "mean", na_action = "replace")},
  {wecoma22 = get_motifels(list(x, y), type = "wecoma", directions = my_dir,
                          size = 100, shift = 100, f = function(){},
                          threshold = 0.5, classes = my_classes,
                          fun = "mean", na_action = "replace")}
)
wecoma_bench2[2:5]


#incoma
incoma_bench2 = bench::mark(
  {incoma21 = motif:::get_motifels_incoma(list(x, y), directions = my_dir,
                                         size = 500, shift = 500,
                                         threshold = 0.5, classes = my_classes)},
  {incoma22 = get_motifels(list(x, y), type = "incoma", directions = my_dir,
                          size = 500, shift = 500, f = function(){},
                          threshold = 0.5, classes = my_classes,
                          fun = NA_character_, na_action = NA_character_)},
  iterations = 1
)
incoma_bench2[2:5]

#composition
composition_bench2 = bench::mark(
  {composition21 = motif:::get_motifels_composition(x,
                                                   size = 100, shift = 100,
                                                   threshold = 0.5, classes = my_classes[1])},
  {composition22 = get_motifels(list(x), type = "composition", directions = matrix(NA),
                               size = 100, shift = 100, f = function(){},
                               threshold = 0.5, classes = my_classes[1],
                               fun = NA_character_, na_action = NA_character_)}
)
composition_bench2[2:5]

#fun2
fun_bench2 = bench::mark(
  {fun21 = motif:::get_motifels_fun(list(x),
                                   size = 500, shift = 500,
                                   f = my_fun,
                                   threshold = 0.5, classes = my_classes[1])},
  {fun22 = get_motifels(list(x), type = "fun", directions = matrix(NA),
                       size = 500, shift = 500,
                       f = my_fun,
                       threshold = 0.5, classes = my_classes[1],
                       fun = NA_character_, na_action = NA_character_)},
  iterations = 2
)
fun_bench2[2:5]
*/
