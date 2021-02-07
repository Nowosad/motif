#include "get_polygons.h"

// https://stackoverflow.com/questions/31691130/conversion-of-r-matrices-to-armadillo-is-really-slow

// [[Rcpp::export]]
List get_polygons(const List& input,
                  std::string type,
                  const arma::imat& m,
                  const arma::imat& directions,
                  Function f,
                  double threshold,
                  const std::string fun,
                  const std::string na_action,
                  List classes) {

  int num_l = input.length();

  arma::ivec classes_m = unique(m.elem(find(m != INT_MIN)));

  //arma::ivec classes_m = unique(m);
  //classes_m = classes_m.elem(find(classes_m != NA_INTEGER));
  int classes_m_size = classes_m.size();

  // initialize objects
  List result(classes_m_size);
  arma::uvec ind_classes_m;
  arma::uvec ind_not_classes_m;
  arma::umat coords_classes_m;
  arma::imat submatrix_x;
  arma::imat submatrix_y;
  arma::dmat submatrix_w;
  arma::imat submatrix_m;
  NumericVector na_perc(classes_m_size);
  NumericVector na_perc_all(num_l);
  List polygons_list(num_l);
  arma::imat x;
  arma::imat y;
  arma::dmat w;

  if ((type == "coma" || type == "cocoma") || (type == "wecoma" || type == "composition")){
    x = Rcpp::as<arma::imat>(input(0));
  }
  if (type == "cocoma"){
    y = Rcpp::as<arma::imat>(input(1));
  }
  if (type == "wecoma"){
    w = Rcpp::as<arma::dmat>(input(1));
  }

  // for each class in m
  for (int i = 0; i < classes_m_size; i++){

    // get class ind
    ind_classes_m = find(m == classes_m[i]);

    // convert class ind to coords (x, y)
    coords_classes_m = arma::ind2sub(size(m), ind_classes_m);
    arma::ucolvec min_coords = arma::min(coords_classes_m, 1);
    arma::ucolvec max_coords = arma::max(coords_classes_m, 1);

    // extract bounding box coords
    unsigned int min_row = min_coords(0);
    unsigned int max_row = max_coords(0);
    unsigned int min_col = min_coords(1);
    unsigned int max_col = max_coords(1);

    // create a submatrix of m
    submatrix_m = m.submat(min_row, min_col, max_row, max_col);
    // get ind of other classes in a submatrix of y
    ind_not_classes_m = find(submatrix_m != classes_m[i]);
    // replace other values by NA
    arma::ivec replacer(ind_not_classes_m.size());
    replacer.fill(NA_INTEGER);

    if (type == "coma"){
      // create a submatrix of x
      submatrix_x = x.submat(min_row, min_col, max_row, max_col);
      submatrix_x.elem(ind_not_classes_m) = replacer;

      // calculate share of NAs
      na_perc(i) = na_prop_polygon(submatrix_x, ind_not_classes_m.size());
      // std::cout << "na prop:   " << na_perc(i) << std::endl;

      // wrap used here is very memory costly
      // try to think how it can be fixed
      // IntegerMatrix p = wrap(submatrix_x);
      // calculate coma
      if (na_perc(i) <= threshold){
        result[i] = comat::rcpp_get_coma_internal(wrap(submatrix_x), directions, classes(0));
      }
    } else if (type == "cocoma"){
      // create a submatrix of x
      submatrix_x = x.submat(min_row, min_col, max_row, max_col);
      submatrix_x.elem(ind_not_classes_m) = replacer;

      // calculate share of NAs
      na_perc(i) = na_prop_polygon(submatrix_x, ind_not_classes_m.size());
      // std::cout << "na prop:   " << na_perc(i) << std::endl;

      if (na_perc(i) <= threshold){
        submatrix_y = y.submat(min_row, min_col, max_row, max_col);
        result[i] = comat::rcpp_get_cocoma_internal(wrap(submatrix_x), wrap(submatrix_y), directions, classes(0), classes(1));
      }

    } else if (type == "wecoma"){
      // create a submatrix of x
      submatrix_x = x.submat(min_row, min_col, max_row, max_col);
      submatrix_x.elem(ind_not_classes_m) = replacer;
      // calculate share of NAs
      na_perc(i) = na_prop_polygon(submatrix_x, ind_not_classes_m.size());
      // std::cout << "na prop:   " << na_perc(i) << std::endl;

      if (na_perc(i) <= threshold){
        submatrix_w = w.submat(min_row, min_col, max_row, max_col);
        result[i] = comat::rcpp_get_wecoma_internal(wrap(submatrix_x), wrap(submatrix_w), directions, classes(0), fun, na_action);
      }

    } else if (type == "incoma"){
      arma::imat one_layer = submatrix_m;

      for (int l = 0; l < num_l; l++){
        arma::imat layer_l = input(l);
        one_layer = layer_l.submat(min_row, min_col, max_row, max_col);
        one_layer.elem(ind_not_classes_m) = replacer;

        polygons_list(l) = one_layer;
        na_perc_all(l) = na_prop_polygon(one_layer, ind_not_classes_m.size());
      }

      na_perc(i) = max(na_perc_all);

      if (na_perc(i) <= threshold){
        result[i] = comat::rcpp_get_incoma_list(polygons_list, directions, classes);
        result[i] = comat::rcpp_get_incoma_matrix(result[i]);
      }
    } else if (type == "composition"){
      // create a submatrix of x
      submatrix_x = x.submat(min_row, min_col, max_row, max_col);
      submatrix_x.elem(ind_not_classes_m) = replacer;

      // calculate share of NAs
      na_perc(i) = na_prop_polygon(submatrix_x, ind_not_classes_m.size());
      // std::cout << "na prop:   " << na_perc(i) << std::endl;

      // wrap used here is very memory costly
      // try to think how it can be fixed
      // IntegerMatrix p = wrap(submatrix_x);
      // calculate coma
      if (na_perc(i) <= threshold){
        result[i] = get_composition(wrap(submatrix_x), classes(0));
      }
    } else if (type == "fun"){
      arma::imat one_layer = submatrix_m;

      for (int l = 0; l < num_l; l++){
        arma::imat layer_l = input(l);
        one_layer = layer_l.submat(min_row, min_col, max_row, max_col);
        one_layer.elem(ind_not_classes_m) = replacer;

        polygons_list(l) = one_layer;
        na_perc_all(l) = na_prop_polygon(one_layer, ind_not_classes_m.size());
      }

      na_perc(i) = max(na_perc_all);

      if (na_perc(i) <= threshold){
        result[i] = f(polygons_list);
      }
    }
  }

  LogicalVector na_perc_below_thres = na_perc <= threshold;
  IntegerVector ids = (wrap(classes_m));
  List df = List::create(Named("id") = ids[na_perc_below_thres],
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
x = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
y = read_stars(system.file("raster/landform.tif", package = "motif"))
x_delta_row = stars::st_dimensions(x)[[1]][["delta"]]
x_delta_col = stars::st_dimensions(x)[[2]][["delta"]]
window = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
window = stars::st_rasterize(window[1],
                             template = stars::st_as_stars(sf::st_bbox(x), values = NA_integer_, dx = x_delta_row, dy = x_delta_col))
window = lapply(window, function(x) `mode<-`(x, "integer"))
x = x[[1]]
y = y[[1]]
mode(x) = "integer"
mode(y) = "integer"
my_classes_x = motif:::get_unique_values(x, TRUE)
my_classes_y = motif:::get_unique_values(y, TRUE)
my_classes = list(my_classes_x, my_classes_y)

my_fun = function(x) mean(x[[1]], na.rm = TRUE)

# coma
coma_bench = bench::mark(
  {coma1 = motif:::get_polygons_coma(x, m = window[[1]], directions = my_dir,
                                     threshold = 0.5, classes = my_classes[1])},
  {coma2 = get_polygons(list(x), type = "coma", m = window[[1]], directions = my_dir,
                        f = function(){}, threshold = 0.5, classes = my_classes[1],
                        fun = NA_character_, na_action = NA_character_)}
)
coma_bench[2:5]

# cocoma
cocoma_bench = bench::mark(
  {cocoma1 = motif:::get_polygons_cocoma(x, y, m = window[[1]], directions = my_dir,
                                     threshold = 0.5, classes = my_classes)},
  {cocoma2 = get_polygons(list(x, y), type = "cocoma", m = window[[1]], directions = my_dir,
                          f = function(){}, threshold = 0.5, classes = my_classes,
                        fun = NA_character_, na_action = NA_character_)}
)
cocoma_bench[2:5]

# wecoma
wecoma_bench = bench::mark(
  {wecoma1 = motif:::get_polygons_wecoma(x, y, m = window[[1]], directions = my_dir,
                                         threshold = 0.5, classes = my_classes,
                                         fun = "mean", na_action = "replace")},
  {wecoma2 = get_polygons(list(x, y), type = "wecoma", m = window[[1]], directions = my_dir,
                          f = function(){}, threshold = 0.5, classes = my_classes,
                          fun = "mean", na_action = "replace")}
)
wecoma_bench[2:5]

# incoma
incoma_bench = bench::mark(
  {incoma1 = motif:::get_polygons_incoma(list(x, y), m = window[[1]], directions = my_dir,
                                     threshold = 0.5, classes = my_classes)},
  {incoma2 = get_polygons(list(x, y), type = "incoma", m = window[[1]], directions = my_dir,
                          f = function(){}, threshold = 0.5, classes = my_classes,
                        fun = NA_character_, na_action = NA_character_)}
)
incoma_bench[2:5]

# composition
composition_bench = bench::mark(
  {composition1 = motif:::get_polygons_composition(x, m = window[[1]],
                                     threshold = 0.5, classes = my_classes[1])},
  {composition2 = get_polygons(list(x), type = "composition", m = window[[1]],
                               directions = matrix(NA), f = function(){},
                               threshold = 0.5, classes = my_classes[1],
                               fun = NA_character_, na_action = NA_character_)}
)
composition_bench[2:5]

# fun
fun_bench = bench::mark(
  {fun1 = motif:::get_polygons_fun(list(x), m = window[[1]],
                                   f = my_fun,
                                   threshold = 0.5, classes = my_classes[1])},
  {fun2 = get_polygons(list(x), type = "fun", m = window[[1]], directions = my_dir,
                               f = my_fun, threshold = 0.5, classes = my_classes[1],
                               fun = NA_character_, na_action = NA_character_)}
)
fun_bench[2:5]
*/
