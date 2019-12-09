#' Calculate Distance Matrix on COMA
#'
#' Calculates a distance matrix based on an object of class `cove`, `cocove`, `wecove`, or `incove`
#'
#' @param x An object of class `cove`, `cocove`, `wecove`, or `incove`
#' @param dist_fun A distance/dissimilarity method used. All possible values can be found using
#' the [philentropy::getDistMethods()] function
#' @param unit A character string specifying the logarithm unit that should be used to
#' compute distances that depend on log computations: `"log", "log2", "log10"`.
#' The default is `"log"`.
#' @param p Power of the Minkowski distance
#'
#' @return An object of class `"dist"``
#'
#' @aliases lsp_to_dist
#' @rdname lsp_to_dist
#'
#' @examples
#' library(comat)
#' library(stars)
#' data(raster_x, package = "comat")
#' raster_x = st_as_stars(raster_x)
#' plot(raster_x)
#'
#' cov = lsp_thumbprint(raster_x, window_size = 2, type = "cove")
#'
#' dist_cov = lsp_to_dist(cov, dist_fun = "jensen-shannon")
#' dist_cov
#' @export
lsp_to_dist = function(x, dist_fun, unit = "log2", p = NULL){
  vec_to_dist(vec = x$signature, dist_fun = dist_fun, unit = unit, p = p)
}

vec_to_dist = function(vec, dist_fun, unit, p){
  mat = do.call(rbind, vec)
  mat = philentropy::distance(mat, method = dist_fun, p = p, test.na = FALSE, unit = unit)
  stats::as.dist(mat)
}
