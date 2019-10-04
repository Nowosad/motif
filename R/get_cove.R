#' Create a co-occurrence vector (wecove)
#'
#' Converts a co-occurrence matrix (wecoma) to
#' a co-occurrence vector (wecove)
#'
#' @param x A matrix - an output of the [get_coma()] function
#' @param ordered The type of pairs considered.
#' Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#'
#' @return A co-occurrence vector
#'
#' @aliases get_cove
#' @rdname get_cove
#'
#' @examples
#' library(comat)
#' library(raster)
#' data(x, package = "comat")
#'
#' com = get_coma(x)
#' com
#'
#' cov = get_cove(com)
#' cov
#'
#' @export
get_cove = function(x, type, normalization) UseMethod("get_cove")

#' @name get_cove
#' @export
get_cove.coma = function(x, type = "ordered", normalization = "none"){
  y = lapply(x$matrix,
             comat:::rcpp_get_cove,
             type = type,
             normalization = normalization)
  x$matrix = NULL
  x$vector = y
  structure(x, class = c("cove", class(x)))
}


# get_cove = function(x, ordered = TRUE){
#   # x = raster::as.matrix(x)
#   y = rcpp_get_vec(x, ordered)
#   structure(y, class = c("numeric", "cove"))
# }
