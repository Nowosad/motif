#' Comparision between spatial patterns
#'
#' Compares two spatial datasets containing categorical raster data.
#' It accepts a categorical raster dataset with one or more attributes, and compares it to the second dataset with the same attributes and dimensions.
#' The both dataset are either compared to as whole areas, areas divided into regular windows (with the `window_size` argument), or areas divided into irregular windows (with the `window` argument).
#' This function allows for several types of comparisons using different representations of spatial patterns, including "coma" (co-occurrence matrix), "cove" (co-occurrence vector), "cocoma" (co-located co-occurrence matrix), "cocove" (co-located co-occurrence vector), "wecoma" (weighted co-occurrence matrix), "wecove" (weighted co-occurrence vector), "incoma" (integrated co-occurrence matrix), "incove" (integrated co-occurrence vector). These representations are created for both datasets, and next a distance between them is calculated using a selected measure from the `philentropy::distance` function.
#' Additional parameters, such as neighbourhood or normalization types, are also available.
#'
#' @param x Object of class `stars` or `stars_proxy`. It should have one attribute (for `"coma"`, `"cove"`), two attributes (`"cocoma"`, `"cocove"`, `"wecoma"`, `"wecove"`), two or more attributes (`"incoma"`, `"incove"`), or any number of attributes suitable for user-defined functions.
#' @param y Object of class `stars` or `stars_proxy`. It should have one attribute (for `"coma"`, `"cove"`), two attributes (`"cocoma"`, `"cocove"`, `"wecoma"`, `"wecove"`), two or more attributes (`"incoma"`, `"incove"`), or any number of attributes suitable for user-defined functions.
#' @param type Type of the calculated signature. It can be `"coma"` (co-occurrence matrix), `"cove"` (co-occurrence vector), `"cocoma"` (co-located co-occurrence matrix), `"cocove"` (co-located co-occurrence vector), `"wecoma"` (weighted co-occurrence matrix), `"wecove"` (weighted co-occurrence vector), `"incoma"` (integrated co-occurrence matrix), `"incove"` (integrated co-occurrence vector), or any function that can summarize `stars` objects.
#' @param dist_fun Distance measure used. This function uses the `philentropy::distance` function in the background. Run `philentropy::getDistMethods()` to find possible distance measures.
#' @param window Specifies areas for analysis. Either `window` or `window_size` argument can be used. An object of class `sf` with one attribute (otherwise, the first attribute is used as an id).
#' @param window_size Specifies areas for analysis. Either `window` or `window_size` argument can be used. Expressed in the numbers of cells, is a length of the side of a square-shaped block of cells. It defines the extent of a local pattern. If `size=NULL` calculations are performed for a whole area.
#' @param window_shift Defines the shift between adjacent squares of cells along with the N-S and W-E directions. It describes the density (resolution) of the output grid. The resolution of the output map will be reduced to the original resolution multiplied by the shift. If shift=size the input map will be divided into a grid of non-overlapping square windows. Each square window defines the extent of a local pattern. If shift < size - results in the grid of overlapping square windows.
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param threshold The share of NA cells to allow metrics calculation.
#' @param ordered For `"cove"`, `"cocove"`, `"wecove"` and `"incove"` only. The type of pairs considered.
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param repeated For `"incove"` only. Should the repeated co-located co-occurrence matrices be used?
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param normalization For `"cove"`, `"cocove"`, `"wecove"` and `"incove"` only. Should the output vector be normalized?
#' Either "none" or "pdf".
#' The "pdf" option normalizes a vector to sum to one.
#' The default is "none".
#' @param wecoma_fun For `"wecoma"` and `"wecove"` only. Function to calculate values from adjacent cells to contribute to exposure matrix, `"mean"` - calculate average values of local population densities from adjacent cells, `"geometric_mean"` - calculate geometric mean values of local population densities from adjacent cells, or `"focal"` assign a value from the focal cell
#' @param wecoma_na_action For `"wecoma"` and `"wecove"` only. Decides on how to behave in the presence of missing values in `w`. Possible options are `"replace"`, `"omit"`, `"keep"`. The default, `"replace"`, replaces missing values with 0, `"omit"` does not use cells with missing values, and `"keep"` keeps missing values.
#' @param ... Additional arguments for the `philentropy::distance` function.
#'
#' @return Object of class `stars`.
#' It has four attributes:
#' (1) `id` - an id of each window.
#' For irregular windows, it is the values provided in the `window` argument,
#' (2) `na_prop_x` - share (0-1) of `NA` cells for each window in the `x` object,
#' (3) `na_prop_y` - share (0-1) of `NA` cells for each window in the `y` object,
#' (4) `dist`- calculated distance between signatures for each window
#'
#' @export
#'
#' @examples
#' library(stars)
#'
#' lc15 = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#' lc01 = read_stars(system.file("raster/landcover2001.tif", package = "motif"))
#' ecoregions = read_stars(system.file("raster/ecoregions.tif", package = "motif"))
#'
#' c1 = lsp_compare(lc01, lc15, type = "cove",
#'     dist_fun = "jensen-shannon", window = ecoregions["id"])
lsp_compare = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                       neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                       normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace", ...) UseMethod("lsp_compare")

#' @name lsp_compare
#' @export
lsp_compare.stars = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
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

  unit = "log2"

  output$dist = mapply(
    distance2,
    output_x$signature,
    output_y$signature,
    method = dist_fun,
    unit = unit,
    ...
  )

  message("Metric: '", dist_fun, "' using unit: '", unit, "'.")

  output_stars = lsp_add_stars(x_metadata,
                                 window = window,
                                 window_size = window_size, window_shift = window_shift)

  output_stars$na_prop_x = output$na_prop_x[match(output_stars$id, output$id)]
  output_stars$na_prop_y = output$na_prop_y[match(output_stars$id, output$id)]
  output_stars$dist = output$dist[match(output_stars$id, output$id)]

  return(output_stars)
}

#' @name lsp_compare
#' @export
lsp_compare.stars_proxy = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                             neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                             normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace", ...){

  x_metadata = stars::st_dimensions(x)

  # prepare window ----------------------------------------------------------
  if (missing(window) || is.null(window)){
    if (is.null(window_size)){
      window_size = 0
    }
  }

  #test if x == y

  classes_x = get_unique_values_proxy(x,
                                      ifelse(is.null(window_size),
                                             ceiling(nrow(x) / nrow(window)),
                                             window_size),
                                      nrow(x), ncol(x))
  classes_y = get_unique_values_proxy(y,
                                      ifelse(is.null(window_size),
                                             ceiling(nrow(y) / nrow(window)),
                                             window_size),
                                      nrow(y), ncol(y))

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

  unit = "log2"

  output$dist = mapply(
    distance2,
    output_x$signature,
    output_y$signature,
    method = dist_fun,
    unit = unit,
    ...
  )

  message("Metric: '", dist_fun, "' using unit: '", unit, "'.")

  output_stars = lsp_add_stars(x_metadata,
                               window = window,
                               window_size = window_size, window_shift = window_shift)

  output_stars$na_prop_x = output$na_prop_x[match(output_stars$id, output$id)]
  output_stars$na_prop_y = output$na_prop_y[match(output_stars$id, output$id)]
  output_stars$dist = output$dist[match(output_stars$id, output$id)]

  return(output_stars)
}
