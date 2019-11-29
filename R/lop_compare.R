#' Title
#'
#' @param x
#' @param y
#' @param type
#' @param dist_fun
#' @param window
#' @param window_size
#' @param window_shift
#' @param neighbourhood
#' @param threshold
#' @param ordered
#' @param repeated
#' @param normalization
#' @param wecoma_fun
#' @param wecoma_na_action
#'
#' @return
#' @export
#'
#' @examples
#' library(raster)
#' library(philentropy)
#' ext = extent(c(xmin = -249797.344531127, xmax = -211162.693944285,
#'                ymin = -597280.143035389, ymax = -558645.492448547))
#'
#' lc = raster(system.file("raster/landcover.tif", package = "lopata"))
#' lf = raster(system.file("raster/landform.tif", package = "lopata"))
#'
#' s1 = lop_compare(list(lc), list(lc), type = "cove", dist_fun = jensen_shannon, threshold = 0.9)
#' s2 = lop_compare(list(lc, lf), list(lc, lf), type = "cocove", dist_fun = jensen_shannon, threshold = 0.9)
#' s3 = lop_compare(list(lc, lf), list(lc, lf), type = "wecove", dist_fun = jensen_shannon, threshold = 0.9)
#' s4 = lop_compare(list(lc, lf), list(lc, lf), type = "incove", dist_fun = jensen_shannon, threshold = 0.9)
lop_compare = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                       neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                       normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace"){

  #test if x == y

  output_x = lop_thumbprint(x, type = type, neighbourhood = neighbourhood, window = window, window_size = window_size, window_shift = window_shift,
                          threshold = threshold, ordered = ordered, repeated = repeated,
                          normalization = normalization, wecoma_fun = wecoma_fun, wecoma_na_action = wecoma_na_action)

  output_y = lop_thumbprint(y, type = type, neighbourhood = neighbourhood, window = window, window_size = window_size, window_shift = window_shift,
                          threshold = threshold, ordered = ordered, repeated = repeated,
                          normalization = normalization, wecoma_fun = wecoma_fun, wecoma_na_action = wecoma_na_action)

  colnames(output_x)[which(colnames(output_x) == "na_prop")] = "na_prop_x"
  colnames(output_y)[which(colnames(output_y) == "na_prop")] = "na_prop_y"

  output = cbind(output_x[c("id", "row", "col", "na_prop_x")], output_y["na_prop_y"])

  output$dist = mapply(philentropy::jensen_shannon, output_x$signature, output_y$signature, testNA = FALSE, unit = "log10")
  return(output)
}
