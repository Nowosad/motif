#' Create a co-occurrence matrix (coma)
#'
#' @param x A RasterLayer with categories
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param size Expressed in the numbers of cells, is a length of the side of a square-shaped block of cells. It defines the extent of a local pattern. If `size=NULL` calculations are performed for a whole area
#' @param shift Defines the shift between adjacent squares of cells along with the N-S and W-E directions. It describes the density (resolution) of the output grid. The resolution of the output map will be reduced to the original resolution multiplied by the shift. If shift=size the input map will be divided into a grid of non-overlapping square windows. Each square window defines the extent of a local pattern. If shift < size - results in the grid of overlapping square windows.
#'
#' @return A co-occurrence matrix
#'
#' @aliases get_coma
#' @rdname get_coma
#'
#' @examples
#' library(comat)
#' library(raster)
#' data(x, package = "comat")
#' plot(x)
#'
#' com = get_coma(x)
#' com
#'
#' com2 = get_coma(x, neighbourhood = 4, size = 2, shift = 2)
#' com2
#'
#' @export
get_coma = function(x, neighbourhood, size, shift) UseMethod("get_coma")

#' @name get_coma
#' @export
get_coma.RasterLayer = function(x, neighbourhood = 4, size = NULL, shift = NULL){
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

# get_coma = function(x, neighbourhood = 4){
#   x = raster::as.matrix(x)
#   directions = as.matrix(neighbourhood)
#
#   n = rcpp_get_coma(x, directions)
#   structure(n, class = c(class(n), "coma"))
# }
