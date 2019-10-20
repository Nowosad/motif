#' Create a co-occurrence vector (cove)
#'
#' Converts a co-occurrence matrix (coma) to
#' a co-occurrence vector (cove)
#'
#' @param x A matrix - an output of the [lop_coma()] function
#' @param ordered The type of pairs considered.
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param normalization Should the output vector be normalized?
#' Either "none" or "pdf".
#' The "pdf" option normalizes a vector to sum to one.
#' The default is "none".
#'
#' @return A co-occurrence vector
#'
#' @aliases lop_cove
#' @rdname lop_cove
#'
#' @examples
#' library(comat)
#' library(raster)
#' landcover = raster(system.file("raster/landcover.tif", package = "lopata"))
#' # plot(landcover)
#'
#' com = lop_coma(landcover, size = 100)
#' com
#'
#' cov = lop_cove(com)
#' cov
#'
#' @export
lop_cove = function(x, ordered, normalization) UseMethod("lop_cove")

#' @name lop_cove
#' @export
lop_cove.coma = function(x, ordered = TRUE, normalization = "none"){
  y = lapply(x$matrix,
             comat::get_cove,
             ordered = ordered,
             normalization = normalization)
  x$matrix = NULL
  x$vector = y
  structure(x, class = c("cove", class(x)))
}


# lop_cove = function(x, ordered = TRUE){
#   # x = raster::as.matrix(x)
#   y = rcpp_lop_vec(x, ordered)
#   structure(y, class = c("numeric", "cove"))
# }
