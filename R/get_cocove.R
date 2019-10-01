#' Create a co-located co-occurrence vector (cocove)
#'
#' Converts a co-located co-occurrence matrix (cocoma) to
#' a co-located co-occurrence vector (cocove)
#'
#' @param x A matrix - an output of the [get_cocoma()] function
#' @param ordered The type of pairs considered.
#' Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#'
#' @return A co-located co-occurrence vector
#' @export
#'
#' @examples
#' library(comat)
#' library(raster)
#' data(x, package = "comat")
#' data(x_na, package = "comat")
#' plot(x)
#' plot(x_na)
#'
#' coom = get_cocoma(x, x_na)
#' coom
#'
#' coov = get_cocove(coom)
#' coov
get_cocove = function(x, ordered = TRUE){
  x = raster::as.matrix(x)
  y = rcpp_get_vec(x, ordered)
  structure(y, class = c("numeric", "cocove"))
}
