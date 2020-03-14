#' Creates a spatial signature
#'
#' Calculates selected spatial signatures based on categorical raster data. It also allows for calculations for any defined regular and irregular areas. It has several built-in signatures but also allows for any user-defined functions.
#'
#' @param x Object of class `stars` or `stars_proxy`. It should have one attribute (for `"coma"`, `"cove"`), two attributes (`"cocoma"`, `"cocove"`, `"wecoma"`, `"wecove"`), two or more attributes (`"incoma"`, `"incove"`), or any number of attributes suitable for user-defined functions.
#' @param type Type of the calculated signature. It can be `"coma"` (co-occurrence matrix), `"cove"` (co-occurrence vector), `"cocoma"` (co-located co-occurrence matrix), `"cocove"` (co-located co-occurrence vector), `"wecoma"` (weighted co-occurrence matrix), `"wecove"` (weighted co-occurrence vector), `"incoma"` (integrated co-occurrence matrix), `"incove"` (integrated co-occurrence vector), or any function that can summarize `stars` objects.
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
#' @param classes Which classes (categories) should be analyzed? This parameter expects a list of the same length as the number of attributes in `x`, where each element of the list contains integer vector. The default is `NULL`, which means that the classes are calculated directly from the input data and all of them are used in the calculations.
#'
#' @return Object of class `lsp`.
#' It has three columns: (1) `id` - an id of each window.
#' For irregular windows, it is the values provided in the `window` argument,
#' (2) `na_prop` - share (0-1) of `NA` cells for each window,
#' (3) `signature` - a list-column containing with calculated signatures
#' @export
#'
#' @examples
#' library(stars)
#'
#' landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#' #plot(landcover)
#' #landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' #plot(landform)
#' #ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
#' #plot(ecoregions["id"])
#'
#' lsp_thumbprint(landcover, type = "coma", threshold = 0.9)
#' lsp_thumbprint(landcover, type = "composition", threshold = 0.9)
#' #lsp_thumbprint(landcover, type = "cove", threshold = 0.9)
#' #lsp_thumbprint(landcover, type = "cove", threshold = 0.9, classes = 10)
#' #lsp_thumbprint(c(landcover, landform), type = "incove", threshold = 0.9)
#'
#' #lsp_thumbprint(landcover, type = "coma", window_size = 100, window_shift = 100, threshold = 0.9)
#' #lsp_thumbprint(landcover, type = "composition",
#' #    window_size = 100, window_shift = 100, threshold = 0.9)
#'
#' #lsp_thumbprint(landcover, type = "coma", window = ecoregions["id"], threshold = 0.9)
lsp_thumbprint = function(x, type, window = NULL, window_size = NULL, window_shift = NULL,
                                      neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                                      normalization = "none", wecoma_fun = "mean", wecoma_na_action = "replace",
                                      classes = NULL){

# get metadata ------------------------------------------------------------
  x_crs = sf::st_crs(x)
  x_bb = sf::st_bbox(x)
  x_delta_row = stars::st_dimensions(x)[[1]][["delta"]]
  x_delta_col = stars::st_dimensions(x)[[2]][["delta"]]

  nc = ncol(x)
  nr = nrow(x)

  directions = as.matrix(neighbourhood)

# prepare window ----------------------------------------------------------
  if (missing(window) || is.null(window)){
    if (is.null(window_size)){
      window_size = 0
    }
    if (missing(window_shift) || is.null(window_shift)){
      window_shift = window_size
    }
  } else {
    if (inherits(x, "stars_proxy")){
      # stop("window option is not implemented for stars_proxy yet", .call = FALSE)
    } else {
      # check for one column
      window = stars::st_rasterize(window[1],
                                   template = stars::st_as_stars(sf::st_bbox(x),
                                                                 values = NA_integer_,
                                                                 dx = x_delta_row,
                                                                 dy = x_delta_col))
      window = lapply(window, function(x) `mode<-`(x, "integer"))
      window = window[[1]]
    }
  }

# prepare classes ---------------------------------------------------------
  if (is.null(classes)){
    if (inherits(x, "stars_proxy")){
      classes = get_unique_values_proxy(x,
                                        ifelse(is.null(window_size),
                                               ceiling(nr / nrow(window)),
                                               window_size),
                                        nr, nc)
    } else {
      classes = lapply(x, get_unique_values, TRUE)
    }
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
  if (missing(window) || is.null(window)){
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
                     nr = nr,
                     nc = nc)

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
  attr(x, "metadata") = c(attr(x, "metadata"),
                          crs = list(x_crs),
                          bb = list(x_bb),
                          delta_x = x_delta_col,
                          delta_y = x_delta_row,
                          window_size = window_size,
                          window_shift = window_shift,
                          use_window = !(missing(window) || is.null(window)))
  return(structure(x, class = c("lsp", class(x))))
}
