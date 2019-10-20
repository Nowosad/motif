#' Create a co-located co-occurrence matrix (cocoma)
#'
#' @param x A RasterLayer with categories
#' @param y A RasterLayer with categories
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param size Expressed in the numbers of cells, is a length of the side of a square-shaped block of cells. It defines the extent of a local pattern. If `size=NULL` calculations are performed for a whole area
#' @param shift Defines the shift between adjacent squares of cells along with the N-S and W-E directions. It describes the density (resolution) of the output grid. The resolution of the output map will be reduced to the original resolution multiplied by the shift. If shift=size the input map will be divided into a grid of non-overlapping square windows. Each square window defines the extent of a local pattern. If shift < size - results in the grid of overlapping square windows.
#' @param threshold The share of NA cells to allow calculation in a square-shaped window.
#'
#' @return A co-located co-occurrence matrix
#'
#' @aliases lop_cocoma
#' @rdname lop_cocoma
#'
#' @examples
#' library(comat)
#' library(raster)
#' l1 = raster(matrix(sample(1:2, size = 10000, replace = TRUE), ncol = 100))
#' l2 = raster(matrix(sample(c(9, 6, 3), size = 10000, replace = TRUE), ncol = 100))
#'
#' coom = lop_cocoma(l1, l2)
#' coom
#'
#' coom2 = lop_cocoma(l1, l2, size = 10, shift = 10)
#' coom2
#' @export
lop_cocoma = function(x, y, neighbourhood, size, shift, threshold) UseMethod("lop_cocoma")

#' @name lop_cocoma
#' @export
lop_cocoma.RasterLayer = function(x, y, neighbourhood = 4, size = NULL, shift = NULL, threshold = 0.5){
  x = raster::as.matrix(x)
  y = raster::as.matrix(y)
  directions = as.matrix(neighbourhood)

  if (is.null(size)){
    size = 0
  }
  if (missing(shift)){
    shift = size
  }

  n = get_motifels_cocoma(x,
                   y,
                   directions = directions,
                   size = size,
                   shift = shift)
  n = tibble::as_tibble(n)

  # n
  structure(n, class = c("cocoma", class(n)))
}

# get_cocoma = function(x, y, neighbourhood = 4){
#   x = raster::as.matrix(x)
#   y = raster::as.matrix(y)
#   directions = as.matrix(neighbourhood)
#
#   n = rcpp_get_cocoma(x, y, directions)
#   structure(n, class = c(class(n), "cocoma"))
# }
