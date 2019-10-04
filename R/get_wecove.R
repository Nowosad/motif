#' Create a weighted co-occurrence vector (wecove)
#'
#' Converts a weighted co-occurrence matrix (wecoma) to
#' a weighted co-occurrence vector (wecove)
#'
#' @param x A matrix - an output of the [get_wecoma()] function
#' @param type ...
#' @param normalization ...
#'
#' @return A weighted co-occurrence vector
#'
#' @aliases get_wecove
#' @rdname get_wecove
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
#'
#' @export
get_wecove = function(x, type, normalization) UseMethod("get_wecove")

#' @name get_wecove
#' @export
get_wecove.wecoma = function(x, type = "ordered", normalization = "none"){
  y = lapply(x$matrix,
             comat:::rcpp_get_wecove,
             type = type,
             normalization = normalization)
  x$matrix = NULL
  x$vector = y
  structure(x, class = c("wecove", class(x)))
}

# get_wecove = function(x, ordered = TRUE){
#   y = rcpp_get_vec(x, ordered)
#   structure(y, class = c("numeric", "wecove"))
# }
