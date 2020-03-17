#' Search for similar spatial pattern
#'
#' Searches for areas with similar spatial patterns in categorical data.
#' It accepts a categorical raster dataset with one or more attributes, and compares it to the second (usually larger) dataset with the same attributes.
#' The first dataset is either compared to a whole area, areas divided into regular windows (with the `window_size` argument), or areas divided into irregular windows (with the `window` argument) from the second dataset.
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
#' It has three attributes:
#' (1) `id` - an id of each window.
#' For irregular windows, it is the values provided in the `window` argument,
#' (2) `na_prop` - share (0-1) of `NA` cells for each window in the `y` object,
#' (3) `dist`- calculated distance between the `x` object and each window in the `y` object
#' @export
#'
#' @examples
#' library(stars)
#'
#' landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#' plot(landcover)
#'
#' ext = st_bbox(c(xmin = -249797.344531127, xmax = -211162.693944285,
#'                 ymin = -597280.143035389, ymax = -558645.492448547),
#'                 crs = st_crs(lc))
#'
#' ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
#' plot(ecoregions["id"])
#'
#' landcover_ext = landcover[ext]
#' plot(landcover_ext)
#'
#' s1 = lsp_search(landcover_ext, landcover, type = "cove",
#'   dist_fun = "jensen-shannon", threshold = 0.9, window_size = 1000)
#' plot(s1["dist"])
#' s2 = lsp_search(landcover_ext, landcover, type = "cove",
#'   dist_fun = "jensen-shannon", threshold = 0.5, window = ecoregions["id"])
#' plot(s2["dist"])
lsp_search = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                      neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                      normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace", ...) UseMethod("lsp_search")


#'
#' @name lsp_search
#' @export
lsp_search.stars = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                      neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                      normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace", ...){


# get metadata ------------------------------------------------------------
  x_metadata = stars::st_dimensions(x)
  y_metadata = stars::st_dimensions(y)

# prepare window ----------------------------------------------------------
  if (missing(window) || is.null(window)){
    if (is.null(window_size)){
      window_size = 0
    }
  }

# prepare classes ---------------------------------------------------------
  classes_x = lapply(lapply(x, function(x) `mode<-`(x, "integer")),
                     get_unique_values, TRUE)

  if (inherits(y, "stars_proxy")){
    classes_y = get_unique_values_proxy(y,
                                        ifelse(is.null(window_size),
                                               ceiling(nrow(y) / nrow(window)),
                                               window_size),
                                        nrow(y), ncol(y))
  } else {
    y = lapply(y, function(x) `mode<-`(x, "integer"))
    y = stars::st_as_stars(y)
    attr(y, "dimensions") = y_metadata
    classes_y = lapply(y, get_unique_values, TRUE)
  }

  classes = mapply(c, classes_x, classes_y, SIMPLIFY = FALSE)
  classes = lapply(classes, unique)
  classes = lapply(classes, sort)

# y signature -------------------------------------------------------------
  output_y = lsp_thumbprint(
    x = y,
    type = type,
    window = window,
    window_size = window_size,
    window_shift = window_shift,
    neighbourhood = neighbourhood,
    threshold = threshold,
    ordered = ordered,
    repeated = repeated,
    normalization = normalization,
    wecoma_fun = wecoma_fun,
    wecoma_na_action = wecoma_na_action,
    classes = classes
  )

# x signature -------------------------------------------------------------
  output_x = lsp_thumbprint(
    x = x,
    type = type,
    window = NULL,
    window_size = NULL,
    window_shift = NULL,
    neighbourhood = neighbourhood,
    threshold = 1,
    ordered = ordered,
    repeated = repeated,
    normalization = normalization,
    wecoma_fun = wecoma_fun,
    wecoma_na_action = wecoma_na_action,
    classes = classes
  )

# calculate distance ------------------------------------------------------
  unit = "log2"
  output_y$dist = unlist(lapply(
    output_y$signature,
    distance2,
    P = output_x$signature[[1]],
    method = dist_fun,
    unit = unit,
    ...
  ))

  message("Metric: '", dist_fun, "' using unit: '", unit, "'.")

# prepare result ----------------------------------------------------------
  output_y$signature = NULL
  output_stars = lsp_add_stars(y_metadata,
                                 window = window,
                                 window_size = window_size, window_shift = window_shift)

  # output_stars$na_prop = NA
  # output_stars$na_prop[which(output_stars$id %in% output$id)] = output$na_prop
  output_stars$na_prop = output_y$na_prop[match(output_stars$id, output_y$id)]

  # output_stars$dist = NA
  # output_stars$dist[which(output_stars$id %in% output$id)] = output$dist
  output_stars$dist = output_y$dist[match(output_stars$id, output_y$id)]

  return(output_stars)
}
