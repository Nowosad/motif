#' Creates a spatial signature
#'
#' Calculates selected spatial signatures based on categorical raster data. It also allows for calculations for any defined regular and irregular areas. It has several built-in signatures but also allows for any user-defined functions.
#'
#' @param x Object of class `stars`, `stars_proxy`, or terra's `SpatRaster`. It should have one attribute (for `"coma"`, `"cove"`), two attributes (`"cocoma"`, `"cocove"`, `"wecoma"`, `"wecove"`), two or more attributes (`"incoma"`, `"incove"`), or any number of attributes suitable for user-defined functions.
#' @param type Type of the calculated signature. It can be `"coma"` (co-occurrence matrix), `"cove"` (co-occurrence vector), `"cocoma"` (co-located co-occurrence matrix), `"cocove"` (co-located co-occurrence vector), `"wecoma"` (weighted co-occurrence matrix), `"wecove"` (weighted co-occurrence vector), `"incoma"` (integrated co-occurrence matrix), `"incove"` (integrated co-occurrence vector), `"composition"` or any function that can summarize `stars` objects.
#' @param window Specifies areas for analysis. It can be either: `NULL`, a numeric value, or an `sf` object. If `window=NULL` calculations are performed for a whole area. If the `window` argument is numeric, it is a length of the side of a square-shaped block of cells. Expressed in the numbers of cells, it defines the extent of a local pattern. If an `sf` object is provided, each feature (row) defines the extent of a local pattern. The `sf` object should have one attribute (otherwise, the first attribute is used as an id).
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param threshold The share of NA cells (0-1) to allow metrics calculation.
#' @param ordered For `"cove"`, `"cocove"`, `"wecove"` and `"incove"` only. The type of pairs considered.
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param repeated For `"incove"` only. Should the repeated co-located co-occurrence matrices be used?
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param normalization For `"cove"`, `"cocove"`, `"wecove"`, `"incove"`, `"composition"`, or user-provided functions only. Should the output vector be normalized?
#' Either "none" or "pdf".
#' The "pdf" option normalizes a vector to sum to one.
#' The default is "pdf".
#' @param wecoma_fun For `"wecoma"` and `"wecove"` only. Function to calculate values from adjacent cells to contribute to exposure matrix, `"mean"` - calculate average values of local population densities from adjacent cells, `"geometric_mean"` - calculate geometric mean values of local population densities from adjacent cells, or `"focal"` assign a value from the focal cell
#' @param wecoma_na_action For `"wecoma"` and `"wecove"` only. Decides on how to behave in the presence of missing values in `w`. Possible options are `"replace"`, `"omit"`, `"keep"`. The default, `"replace"`, replaces missing values with 0, `"omit"` does not use cells with missing values, and `"keep"` keeps missing values.
#' @param classes Which classes (categories) should be analyzed? This parameter expects a list of the same length as the number of attributes in `x`, where each element of the list contains integer vector. The default is `NULL`, which means that the classes are calculated directly from the input data and all of them are used in the calculations.
#'
#' @return Object of class `lsp`.
#' It has three columns: (1) `id` - an id of each window.
#' For irregular windows, it is the values provided in the `window` argument,
#' (2) `na_prop` - share (0-1) of `NA` cells for each window,
#' (3) `signature` - a list-column containing calculated signatures
#' @export
#'
#' @examples
#' library(stars)
#'
#' landcover = read_stars(system.file("raster/landcover2015s.tif", package = "motif"))
#'
#' landcover_coma = lsp_signature(landcover, type = "coma", threshold = 0.9, window = 2000)
#' landcover_coma
#'
#' landcover_comp = lsp_signature(landcover, type = "composition", threshold = 0.9)
#' landcover_comp
#'
#' \donttest{
#' # larger data example
#' library(stars)
#'
#' landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#'
#' landcover_coma = lsp_signature(landcover, type = "coma", threshold = 0.9, window = 2000)
#' landcover_coma
#'
#' landcover_comp = lsp_signature(landcover, type = "composition", threshold = 0.9)
#' landcover_comp
#' }
lsp_signature = function(x, type, window = NULL, neighbourhood = 4, threshold = 0.9, ordered = TRUE, repeated = TRUE, normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace", classes = NULL){
  if (inherits(x, "SpatRaster")){
    x = stars::st_as_stars(x)
  }

# get metadata ------------------------------------------------------------
  x_crs = sf::st_crs(x)
  x_bb = sf::st_bbox(x)
  x_delta_row = stars::st_dimensions(x)[[1]][["delta"]]
  x_delta_col = stars::st_dimensions(x)[[2]][["delta"]]

  nc = ncol(x)
  nr = nrow(x)

  directions = as.matrix(neighbourhood)

# prepare window ----------------------------------------------------------
  if (is.null(window) && inherits(x, "stars_proxy")){
    x = stars::st_as_stars(x)
  }

  window_meta = prepare_window(x, window)
  window = window_meta[["window"]]
  window_size = window_meta[["window_size"]]
  window_shift = window_meta[["window_shift"]]

# prepare classes ---------------------------------------------------------
  if (is.null(classes)){
    classes = determine_classes(x, window)
  }
  if (inherits(classes, "numeric") || inherits(classes, "integer")){
    classes = list(classes)
  }

# prepare type ------------------------------------------------------------
  if (is.function(type)){
    f = type; type = "fun"
  } else {
    f = function(){}
  }

# calculate ---------------------------------------------------------------
  if ((is.vector(window) && is.numeric(window)) || is.null(window)){
    if (type == "composition" && length(x) > 1){
      warning("Only the first layer will be used", call. = FALSE)
    }
    x = get_motifels_all(x,
                     type = type,
                     directions = directions,
                     window_size = window_size,
                     window_shift = window_shift,
                     f = f,
                     threshold = threshold,
                     classes = classes,
                     ordered = ordered,
                     repeated = repeated,
                     normalization = normalization,
                     wecoma_fun = wecoma_fun,
                     wecoma_na_action = wecoma_na_action,
                     dimensions = stars::st_dimensions(x))

  } else {
    x = get_polygons_all(x,
                         type = type,
                         directions = directions,
                         window = window,
                         f = f,
                         threshold = threshold,
                         classes = classes,
                         ordered = ordered,
                         repeated = repeated,
                         normalization = normalization,
                         wecoma_fun = wecoma_fun,
                         wecoma_na_action = wecoma_na_action)
  }

# add spatial metadata ----------------------------------------------------
  if (!is.null(x)){
    attr(x, "metadata") = c(attr(x, "metadata"),
                            crs = list(x_crs),
                            bb = list(x_bb),
                            delta_x = x_delta_col,
                            delta_y = x_delta_row,
                            window_size = window_size,
                            window_shift = window_shift,
                            use_window = inherits(window, "matrix"))
    return(structure(x, class = c("lsp", class(x))))
  }
}
