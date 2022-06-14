#' Prepares window* arguments (internal function)
#'
#' @param x Object of class `stars` or `stars_proxy`
#' @param window Specifies areas for analysis. It can be either: `NULL`, a numeric value, or an `sf` object. If `window=NULL` calculations are performed for a whole area. If the `window` argument is numeric, it is a length of the side of a square-shaped block of cells. Expressed in the numbers of cells, it defines the extent of a local pattern. If an `sf` object is provided, each feature (row) defines the extent of a local pattern. The `sf` object should have one attribute (otherwise, the first attribute is used as an id).
#'
#' @return A list with `window`, `window_size`, and `window_shift`
prepare_window = function(x, window){
  if (is.null(window)){
    window_size = 0
    window_shift = window_size
  } else if (is.numeric(window)){
    if (length(window) == 1){
      window_size = window
      window_shift = window_size
    } else {
      window_size = window[[1]]
      window_shift = window[[2]]
      window = window[[1]]
    }
  } else {
    window_size = NULL
    window_shift = NULL
    if (inherits(x, "stars_proxy")){
      # stop("window option is not implemented for stars_proxy yet", .call = FALSE)
    } else {
      # check for one column
      window = stars::st_rasterize(window[1],
                                   template = stars::st_as_stars(sf::st_bbox(x),
                                                                 values = NA_integer_,
                                                                 dx = stars::st_dimensions(x)[[1]][["delta"]],
                                                                 dy = stars::st_dimensions(x)[[2]][["delta"]]))
      window = lapply(window, function(x) `mode<-`(x, "integer"))
      window = window[[1]]
    }
  }
  return(list(window = window,
              window_size = window_size,
              window_shift = window_shift))
}
