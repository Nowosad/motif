#' Create a co-occurrence matrix (coma)
#'
#' @param x A RasterLayer with categories
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param size Expressed in the numbers of cells, is a length of the side of a square-shaped block of cells. It defines the extent of a local pattern. If `size=NULL` calculations are performed for a whole area
#' @param shift Defines the shift between adjacent squares of cells along with the N-S and W-E directions. It describes the density (resolution) of the output grid. The resolution of the output map will be reduced to the original resolution multiplied by the shift. If shift=size the input map will be divided into a grid of non-overlapping square windows. Each square window defines the extent of a local pattern. If shift < size - results in the grid of overlapping square windows.
#' @param threshold The share of NA cells to allow calculation in a square-shaped window.
#'
#' @return A co-occurrence matrix
#'
#' @aliases lop_coma
#' @rdname lop_coma
#'
#' @examples
#' library(comat)
#' library(raster)
#' landcover = raster(system.file("raster/landcover.tif", package = "lopata"))
#' # plot(landcover)
#'
#' com = lop_coma(landcover)
#' com
#'
#' com2 = lop_coma(landcover, size = 100)
#' com2
#'
#' @export
lop_coma = function(x, neighbourhood, size, shift, threshold) UseMethod("lop_coma")

#' @name lop_coma
#' @export
lop_coma.RasterLayer = function(x, neighbourhood = 4, size = NULL, shift = NULL, threshold = 0.5){
  x = raster::as.matrix(x)
  directions = as.matrix(neighbourhood)

  if (is.null(size)){
    size = 0
  }
  if (missing(shift)){
    shift = size
  }

  n = get_motifels_coma(x,
                        directions = directions,
                        size = size,
                        shift = shift)
  n = tibble::as_tibble(n)
  structure(n, class = c("coma", class(n)))
}

# lop_coma = function(x, neighbourhood = 4){
#   x = raster::as.matrix(x)
#   directions = as.matrix(neighbourhood)
#
#   n = rcpp_lop_coma(x, directions)
#   structure(n, class = c(class(n), "coma"))
# }
