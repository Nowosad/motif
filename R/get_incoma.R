#' Create an integrated co-occurrence matrix (wecoma)
#'
#' @param x A RasterStack object containing categorical rasters
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#'
#' @return An integrated co-occurrence matrix
#'
#' @aliases get_incoma
#' @rdname get_incoma
#'
#' @examples
#' library(raster)
#'
#' set.seed(10100)
#' l1 = raster(matrix(sample(1:2, size = 1000000, replace = TRUE), ncol = 1000))
#' l2 = raster(matrix(sample(c(9, 6, 3), size = 1000000, replace = TRUE), ncol = 1000))
#' x = stack(l1, l2, l1)
#'
#' get_incoma(x)
#' get_incoma(x, size = 100, shift = 100)
#' @export
get_incoma = function(x, neighbourhood, size, shift) UseMethod("get_incoma")

#' @name get_incoma
#' @export
get_incoma.RasterStack = function(x, neighbourhood = 4, size = NULL, shift = NULL){
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
                          shift = shift)
  n = tibble::as_tibble(n)

  # n
  structure(n, class = c("incoma", class(n)))
}

# get_incoma = function(x, neighbourhood = 4){
#   rasters = lapply(raster::as.list(x), raster::as.matrix)
#   directions = as.matrix(neighbourhood)
#
#   n = rcpp_get_incoma(rasters, directions)
#   structure(n, class = c(class(n), "incoma"))
# }
