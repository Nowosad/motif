#' Create a weighted co-occurrence matrix (wecoma)
#'
#' @param x A RasterLayer with categories
#' @param w A RasterLayer with weights
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param size Expressed in the numbers of cells, is a length of the side of a square-shaped block of cells. It defines the extent of a local pattern. If `size=NULL` calculations are performed for a whole area
#' @param shift Defines the shift between adjacent squares of cells along with the N-S and W-E directions. It describes the density (resolution) of the output grid. The resolution of the output map will be reduced to the original resolution multiplied by the shift. If shift=size the input map will be divided into a grid of non-overlapping square windows. Each square window defines the extent of a local pattern. If shift < size - results in the grid of overlapping square windows.
#' @param threshold The share of NA cells to allow calculation in a square-shaped window.
#' @param fun "mean", "geometric_mean", or "focal". The default is "mean".
#' @param na_action "replace", "omit", or "keep". The default is "replace".
#'
#' @return A weighted co-occurrence matrix
#'
#' @aliases lop_wecoma
#' @rdname lop_wecoma
#'
#' @examples
#' library(comat)
#' library(raster)
#' data(raster_x, package = "comat")
#' data(raster_w_na, package = "comat")
#' raster_x = raster(raster_x)
#' raster_w_na = raster(raster_w_na)
#'
#' plot(raster_x)
#' plot(raster_w_na)
#'
#' wom = lop_wecoma(raster_x, raster_w_na)
#' wom
#' wom2 = lop_wecoma(raster_x, raster_w_na, size = 2, shift = 2)
#' wom2
#' @export
lop_wecoma = function(x, w, neighbourhood, size, shift, threshold, fun, na_action) UseMethod("lop_wecoma")

#' @name lop_wecoma
#' @export
lop_wecoma.RasterLayer = function(x, w, neighbourhood = 4, size = NULL, shift = NULL,
                                  threshold = 0.5,
                                  fun = "mean", na_action = "replace"){
  x = raster::as.matrix(x)
  w = raster::as.matrix(w)
  directions = as.matrix(neighbourhood)

  if (is.null(size)){
    size = 0
  }
  if (missing(shift)){
    shift = size
  }

  n = get_motifels_wecoma(x = x,
                   w = w,
                   directions = directions,
                   size = size,
                   shift = shift,
                   fun = fun,
                   na_action = na_action)
  n = tibble::as_tibble(n)

  # n
  structure(n, class = c("wecoma", class(n)))
}

# lop_wecoma = function(x, w, neighbourhood = 4, fun = "mean", na_action = "replace"){
#   x = raster::as.matrix(x)
#   w = raster::as.matrix(w)
#   directions = as.matrix(neighbourhood)
#
#   n = rcpp_lop_wecoma(x, w, directions, fun, na_action)
#   structure(n, class = c(class(n), "wecoma"))
# }
