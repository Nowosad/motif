#' Create an integrated co-occurrence vector (wecove)
#'
#' Converts an integrated co-occurrence matrix (wecoma) to
#' an integrated co-occurrence vector (wecove)
#'
#' @param x A matrix - an output of the [get_incoma()] function
#' @param ordered The type of pairs considered.
#' Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#'
#' @return An integrated co-occurrence vector
#' @export
#'
#' @examples
#' library(comat)
#' library(raster)
#'
#' l1 = raster(matrix(sample(1:2, size = 100, replace = TRUE), ncol = 10))
#' l2 = raster(matrix(sample(c(9, 6, 3), size = 100, replace = TRUE), ncol = 10))
#' x = stack(l1, l2, l1)
#'
#' incom = get_incoma(x)
#'
#' incov1 = get_incove(incom)
#' incov1
#'
#' incov2 = get_incove(incom, ordered = FALSE)
#' incov2
get_incove = function(x, ordered = TRUE){
  x = raster::as.matrix(x)
  y = rcpp_get_vec(x, ordered)
  structure(y, class = c("numeric", "incove"))
}

