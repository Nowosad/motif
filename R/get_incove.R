#' Create an integrated co-occurrence vector (incove)
#'
#' Converts an integrated co-occurrence matrix (incoma) to
#' an integrated co-occurrence vector (incove)
#'
#' @param x A matrix - an output of the [get_incoma()] function
#' @param type ...
#' @param normalization ...
#'
#' @return An integrated co-occurrence vector
#'
#' @aliases get_incove
#' @rdname get_incove
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
#' incov2 = get_incove(incom, type = "unordered")
#' incov2
#'
#' @export
get_incove = function(x, type, normalization) UseMethod("get_incove")

#' @name get_wecove
#' @export
get_incove.incoma = function(x, type = "ordered", normalization = "none"){
  y = lapply(x$matrix,
             comat:::rcpp_get_incove,
             type = type,
             normalization = normalization)
  x$matrix = NULL
  x$vector = y
  structure(x, class = c("incove", class(x)))
}



# get_incove = function(x, ordered = TRUE){
#   x = raster::as.matrix(x)
#   y = rcpp_get_vec(x, ordered)
#   structure(y, class = c("numeric", "incove"))
# }

