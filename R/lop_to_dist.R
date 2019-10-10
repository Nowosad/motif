#' Calculate Distance Matrix on COMA
#'
#' Calculates a distance matrix based on an object of class `cove`, `cocove`, `wecove`, or `incove`
#'
#' @param x An object of class `cove`, `cocove`, `wecove`, or `incove`
#' @param method A distance/dissimilarity method used. All possible values can be found using
#' the [philentropy::getDistMethods()] function
#' @param unit A character string specifying the logarithm unit that should be used to
#' compute distances that depend on log computations: `"log", "log2", "log10"`.
#' The default is `"log"`.
#' @param p Power of the Minkowski distance
#'
#' @return An object of class `"dist"``
#'
#' @aliases lop_to_dist
#' @rdname lop_to_dist
#'
#' @examples
#' library(comat)
#' library(raster)
#' data(raster_x, package = "comat")
#' raster_x = raster(raster_x)
#' plot(raster_x)
#'
#' com = lop_coma(raster_x, size = 2)
#' com
#'
#' cov = lop_cove(com)
#' cov
#'
#' dist_cov = lop_to_dist(cov, method = "jensen-shannon")
#' dist_cov
#' @export
lop_to_dist = function(x, method, unit, p) UseMethod("lop_to_dist")
#'
#' @name lop_to_dist
#' @export
lop_to_dist.cove = function(x, method, unit = "log", p = NULL){
  vec_to_dist(vec = x$vector, method = method, unit = unit, p = p)
}
#' @name lop_to_dist
#' @export
lop_to_dist.cocove = function(x, method, unit = "log", p = NULL){
  vec_to_dist(vec = x$vector, method = method, unit = unit, p = p)
}
#' @name lop_to_dist
#' @export
lop_to_dist.wecove = function(x, method, unit = "log", p = NULL){
  vec_to_dist(vec = x$vector, method = method, unit = unit, p = p)
}
#' @name lop_to_dist
#' @export
lop_to_dist.incoma = function(x, method, unit = "log", p = NULL){
  vec_to_dist(vec = x$vector, method = method, unit = unit, p = p)
}

vec_to_dist = function(vec, method, unit, p){
  mat = do.call(rbind, vec)
  mat = philentropy::distance(mat, method = method, p = p, test.na = FALSE, unit = unit)
  stats::as.dist(mat)
}
