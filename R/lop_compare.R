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
#' library(stars)
#' library(philentropy)
#'
#' lc15 = read_stars(system.file("raster/landcover2015.tif", package = "lopata"))
#' lc01 = read_stars(system.file("raster/landcover2001.tif", package = "lopata"))
#' lf = read_stars(system.file("raster/landform.tif", package = "lopata"))
#' ecoregions = read_stars(system.file("raster/ecoregions.tif", package = "lopata"))
#'
#' s1 = lop_compare(lc01, lc15, type = "cove", dist_fun = jensen_shannon, threshold = 0.9)
#' s1b = lop_compare(lc01, lc15, type = "cove", dist_fun = jensen_shannon, window = ecoregions, threshold = 0.9)
#' s1c = lop_compare(lc01, lc15, type = "cove", dist_fun = jensen_shannon, window_size = 1000, threshold = 0.9)
#' s2 = lop_compare(c(lc01, lf), c(lc15, lf), type = "cocove", dist_fun = jensen_shannon, threshold = 0.9)
#' s3 = lop_compare(c(lc01, lf), c(lc15, lf), type = "wecove", dist_fun = jensen_shannon, threshold = 0.9)
#' s4 = lop_compare(c(lc01, lf), c(lc15, lf), type = "incove", dist_fun = jensen_shannon, threshold = 0.9)
lop_compare = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                       neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                       normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace"){

  #test if x == y

  # use lapply here?
  output_x = lop_thumbprint(
    x,
    type = type,
    neighbourhood = neighbourhood,
    window = window,
    window_size = window_size,
    window_shift = window_shift,
    threshold = threshold,
    ordered = ordered,
    repeated = repeated,
    normalization = normalization,
    wecoma_fun = wecoma_fun,
    wecoma_na_action = wecoma_na_action
  )

  output_y = lop_thumbprint(
    y,
    type = type,
    neighbourhood = neighbourhood,
    window = window,
    window_size = window_size,
    window_shift = window_shift,
    threshold = threshold,
    ordered = ordered,
    repeated = repeated,
    normalization = normalization,
    wecoma_fun = wecoma_fun,
    wecoma_na_action = wecoma_na_action
  )

  colnames(output_x)[which(colnames(output_x) == "na_prop")] = "na_prop_x"
  colnames(output_y)[which(colnames(output_y) == "na_prop")] = "na_prop_y"

  output = cbind(output_x[!names(output_x) == "signature"], output_y["na_prop_y"])

  output$dist = mapply(
    dist_fun,
    output_x$signature,
    output_y$signature,
    testNA = TRUE,
    unit = "log10"
  )
  return(output)
}
