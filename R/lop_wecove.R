#' Create a weighted co-occurrence vector (wecove)
#'
#' Converts a weighted co-occurrence matrix (wecoma) to
#' a weighted co-occurrence vector (wecove)
#'
#' @param x A matrix - an output of the [lop_wecoma()] function
#' @param ordered The type of pairs considered.
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param normalization Should the output vector be normalized?
#' Either "none" or "pdf".
#' The "pdf" option normalizes a vector to sum to one.
#' The default is "none".
#'
#' @return A weighted co-occurrence vector
#'
#' @aliases lop_wecove
#' @rdname lop_wecove
#'
#' @examples
#' library(comat)
#' library(raster)
#' data(raster_x, package = "comat")
#' data(raster_w_na, package = "comat")
#' raster_x = raster(raster_x)
#' raster_w_na = raster(raster_w_na)
#'
#' plot(raster_x)
#' plot(raster_w_na)
#'
#' wom = lop_wecoma(raster_x, raster_w_na)
#' wom
#'
#' wov = lop_wecove(wom)
#' wov
#'
#' @export
lop_wecove = function(x, ordered, normalization) UseMethod("lop_wecove")

#' @name lop_wecove
#' @export
lop_wecove.wecoma = function(x, ordered = TRUE, normalization = "none"){
  y = lapply(x$matrix,
             comat::get_wecove,
             ordered = ordered,
             normalization = normalization)
  x$matrix = NULL
  x$vector = y
  structure(x, class = c("wecove", class(x)))
}

# lop_wecove = function(x, ordered = TRUE){
#   y = rcpp_lop_vec(x, ordered)
#   structure(y, class = c("numeric", "wecove"))
# }
