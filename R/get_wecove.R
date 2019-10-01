#' Create a weighted co-occurrence vector (wecove)
#'
#' Converts a weighted co-occurrence matrix (wecoma) to
#' a weighted co-occurrence vector (wecove)
#'
#' @param x A matrix - an output of the [get_wecoma()] function
#' @param ordered The type of pairs considered.
#' Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#'
#' @return A weighted co-occurrence vector
#' @export
#'
#' @examples
#' library(comat)
#' library(raster)
#' data(x, package = "comat")
#' data(w, package = "comat")
#'
#' wom = get_wecoma(x, w)
#' wom
#'
#' wov = get_wecove(wom)
#' wov
get_wecove = function(x, ordered = TRUE){
  y = rcpp_get_vec(x, ordered)
  structure(y, class = c("numeric", "wecove"))
}
