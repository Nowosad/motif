#' Create an integrated co-occurrence vector (incove)
#'
#' Converts an integrated co-occurrence matrix (incoma) to
#' an integrated co-occurrence vector (incove)
#'
#' @param x A matrix - an output of the [lop_incoma()] function
#' @param ordered The type of pairs considered.
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param repeated Should the repeated co-located co-occurrence matrices be used?
#' Either "ordered" (TRUE) or "unordered" (FALSE).
#' The default is TRUE.
#' @param normalization Should the output vector be normalized?
#' Either "none" or "pdf".
#' The "pdf" option normalizes a vector to sum to one.
#' The default is "none".
#'
#' @return An integrated co-occurrence vector
#'
#' @aliases lop_incove
#' @rdname lop_incove
#'
#' @examples
#' library(comat)
#' library(raster)
#'
#' l1 = raster(matrix(sample(1:2, size = 100, replace = TRUE), ncol = 10))
#' l2 = raster(matrix(sample(c(9, 6, 3), size = 100, replace = TRUE), ncol = 10))
#' x = stack(l1, l2, l1)
#'
#' incom = lop_incoma(x)
#' incom
#'
#' incov1 = lop_incove(incom)
#' incov1
#'
#' incov2 = lop_incove(incom, ordered = FALSE)
#' incov2
#'
#' @export
lop_incove = function(x, ordered, repeated, normalization) UseMethod("lop_incove")

#' @name lop_incove
#' @export
lop_incove.incoma = function(x, ordered = TRUE, repeated = TRUE, normalization = "none"){
  y = lapply(x$matrix,
             comat::get_incove,
             ordered = ordered,
             repeated = repeated,
             normalization = normalization)
  x$matrix = NULL
  x$vector = y
  structure(x, class = c("incove", class(x)))
}



# lop_incove = function(x, ordered = TRUE){
#   x = raster::as.matrix(x)
#   y = rcpp_lop_vec(x, ordered)
#   structure(y, class = c("numeric", "incove"))
# }

