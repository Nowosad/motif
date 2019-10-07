#' Create a co-located co-occurrence vector (cocove)
#'
#' Converts a co-located co-occurrence matrix (cocoma) to
#' a co-located co-occurrence vector (cocove)
#'
#' @param x A matrix - an output of the [lop_cocoma()] function
#' @param ordered The type of pairs considered.
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param normalization Should the output vector be normalized?
#' Either "none" or "pdf".
#' The "pdf" option normalizes a vector to sum to one.
#' The default is "none".
#'
#' @return A co-located co-occurrence vector
#'
#' @aliases lop_cocove
#' @rdname lop_cocove
#'
#' @examples
#' library(lopata)
#' library(raster)
#' data(raster_x, package = "comat")
#' data(raster_x_na, package = "comat")
#' raster_x = raster(raster_x)
#' raster_x_na = raster(raster_x_na)
#'
#' plot(raster_x)
#' plot(raster_x_na)
#'
#' coom = lop_cocoma(raster_x, raster_x_na)
#' coom
#'
#' coov = lop_cocove(coom)
#' coov
#'
#' @export
lop_cocove = function(x, ordered, normalization) UseMethod("lop_cocove")

#' @name lop_cocove
#' @export
lop_cocove.cocoma = function(x, ordered = TRUE, normalization = "none"){
  y = lapply(x$matrix,
             comat::get_cocove,
             ordered = ordered,
             normalization = normalization)
  x$matrix = NULL
  x$vector = y
  structure(x, class = c("cocove", class(x)))
}

# lop_cocove = function(x, type = "ordered", normalization = "none"){
#   x = raster::as.matrix(x)
#   y = comat::rcpp_lop_cocove(x, type, normalization)
#   structure(y, class = c("numeric", "cocove"))
# }
