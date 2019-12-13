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
#'
#' lc15 = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#' lc01 = read_stars(system.file("raster/landcover2001.tif", package = "motif"))
#' lf = read_stars(system.file("raster/landform.tif", package = "motif"))
#' ecoregions = read_stars(system.file("raster/ecoregions.tif", package = "motif"))
#'
#' s1 = lsp_compare(lc01, lc15, type = "cove",
#'  dist_fun = "jensen-shannon", threshold = 0.9)
#' #s1b = lsp_compare(lc01, lc15, type = "cove",
#' #  dist_fun = "jensen-shannon", window = ecoregions, threshold = 0.9)
#' #s1c = lsp_compare(lc01, lc15, type = "cove",
#' #  dist_fun = "jensen-shannon", window_size = 100, threshold = 0.9)
#' #s2 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "cocove",
#' #  dist_fun = "jensen-shannon", threshold = 0.9)
#' #s3 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "wecove",
#' #  dist_fun = "jensen-shannon", threshold = 0.9)
#' #s4 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "incove",
#' #  dist_fun = "jensen-shannon", threshold = 0.9)
#'
lsp_compare = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                       neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                       normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace", ...){

  x_metadata = stars::st_dimensions(x)

  #test if x == y

  classes_x = lapply(x, get_unique_values, TRUE)
  classes_y = lapply(y, get_unique_values, TRUE)

  classes = mapply(c, classes_x, classes_y, SIMPLIFY = FALSE)
  classes = lapply(classes, unique)

  # use lapply here?
  output_x = lsp_thumbprint(
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
    wecoma_na_action = wecoma_na_action,
    classes = classes
  )

  output_y = lsp_thumbprint(
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
    wecoma_na_action = wecoma_na_action,
    classes = classes
  )

  colnames(output_x)[which(colnames(output_x) == "na_prop")] = "na_prop_x"
  colnames(output_y)[which(colnames(output_y) == "na_prop")] = "na_prop_y"

  output = cbind(output_x[!names(output_x) == "signature"], output_y["na_prop_y"])

  # unify signatures
  # attributes(output_x)

  output$dist = mapply(
    distance2,
    output_x$signature,
    output_y$signature,
    method = dist_fun,
    unit = "log2",
    ...
  )

  output_stars = lsp_add_spatial(x_metadata,
                                 window = window,
                                 window_size = window_size, window_shift = window_shift)

  output_stars$na_prop_x = output$na_prop_x[match(output_stars$id, output$id)]
  output_stars$na_prop_y = output$na_prop_y[match(output_stars$id, output$id)]
  output_stars$dist = output$dist[match(output_stars$id, output$id)]

  return(output_stars)
}
