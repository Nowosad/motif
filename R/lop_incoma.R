#' Create an integrated co-occurrence matrix (wecoma)
#'
#' @param x A RasterStack object containing categorical rasters
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param size Expressed in the numbers of cells, is a length of the side of a square-shaped block of cells. It defines the extent of a local pattern. If `size=NULL` calculations are performed for a whole area
#' @param shift Defines the shift between adjacent squares of cells along with the N-S and W-E directions. It describes the density (resolution) of the output grid. The resolution of the output map will be reduced to the original resolution multiplied by the shift. If shift=size the input map will be divided into a grid of non-overlapping square windows. Each square window defines the extent of a local pattern. If shift < size - results in the grid of overlapping square windows.
#' @param threshold The share of NA cells to allow calculation in a square-shaped window.
#'
#' @return An integrated co-occurrence matrix
#'
#' @aliases lop_incoma
#' @rdname lop_incoma
#'
#' @examples
#' library(raster)
#'
#' set.seed(10100)
#' l1 = raster(matrix(sample(1:2, size = 1000000, replace = TRUE), ncol = 1000))
#' l2 = raster(matrix(sample(c(9, 6, 3), size = 1000000, replace = TRUE), ncol = 1000))
#' x = stack(l1, l2, l1)
#'
#' lop_incoma(x)
#' lopata::lop_incoma(x, size = 100, shift = 100)
#' @export
lop_incoma = function(x, neighbourhood, size, shift, threshold) UseMethod("lop_incoma")

#' @name lop_incoma
#' @export
lop_incoma.RasterStack = function(x, neighbourhood = 4, size = NULL, shift = NULL, threshold = 0.5){
  rasters = lapply(raster::as.list(x), raster::as.matrix)
  directions = as.matrix(neighbourhood)

  if (is.null(size)){
    size = 0
  }
  if (missing(shift)){
    shift = size
  }

  n = get_motifels_incoma(rasters,
                          directions = directions,
                          size = size,
                          shift = shift,
                          threshold = threshold)
  n = tibble::as_tibble(n)

  # n
  structure(n, class = c("incoma", class(n)))
}

# lop_incoma = function(x, neighbourhood = 4){
#   rasters = lapply(raster::as.list(x), raster::as.matrix)
#   directions = as.matrix(neighbourhood)
#
#   n = rcpp_lop_incoma(rasters, directions)
#   structure(n, class = c(class(n), "incoma"))
# }
