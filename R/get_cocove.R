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
#'
#' @aliases get_cocove
#' @rdname get_cocove
#'
#' @examples
#' library(lopata)
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
#'
#' @export
get_cocove = function(x, type, normalization) UseMethod("get_cocove")

#' @name get_cocove
#' @export
get_cocove.cocoma = function(x, type = "ordered", normalization = "none"){
  y = lapply(x$matrix,
             comat:::rcpp_get_cocove,
             type = type,
             normalization = normalization)
  x$matrix = NULL
  x$vector = y
  structure(x, class = c("cocove", class(x)))
}

# get_cocove = function(x, type = "ordered", normalization = "none"){
#   x = raster::as.matrix(x)
#   y = comat::rcpp_get_cocove(x, type, normalization)
#   structure(y, class = c("numeric", "cocove"))
# }
