#' Calculate Distance Matrix
#'
#' Calculates a distance matrix based on an object of class `lsp`.
#'
#' @param x An object of class `lsp` - usually the output of
#' the `lsp_signature()` function
#' @param dist_fun A distance/dissimilarity method used.
#' All possible values can be found using
#' the [philentropy::getDistMethods()] function
#' @param unit A character string specifying the logarithm unit
#' that should be used to compute distances that depend on log computations:
#'  `"log", "log2", "log10"`.
#' The default is `"log"`
#' @param p Power of the Minkowski distance.
#' Used only when the `dist_fun = "minkowski"`
#'
#' @return An object of class `"dist"``
#'
#' @aliases lsp_to_dist
#' @rdname lsp_to_dist
#'
#' @examples
#' library(stars)
#' landcover = read_stars(system.file("raster/landcover2015s.tif", package = "motif"))
#'
#' landcover_cove = lsp_signature(landcover, type = "cove", threshold = 0.9, window = 400)
#' landcover_cove
#'
#' dist_cov = lsp_to_dist(landcover_cove, dist_fun = "jensen-shannon")
#' dist_cov
#'
#' \donttest{
#' # larger data example
#' library(stars)
#' landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#'
#' landcover_cove = lsp_signature(landcover, type = "cove", threshold = 0.9, window = 2000)
#' landcover_cove
#'
#' dist_cov = lsp_to_dist(landcover_cove, dist_fun = "jensen-shannon")
#' dist_cov
#' }
#' @export
lsp_to_dist = function(x, dist_fun, unit = "log2", p = NULL){
  if (nrow(x) < 2){
    stop("You need at least two signatures to calculate distances", .call = FALSE)
  }
  vec_to_dist(x, dist_fun = dist_fun, unit = unit, p = p)
}

vec_to_dist = function(x, dist_fun, unit, p){
  mat = do.call(rbind, x$signature)
  mat = philentropy::distance(mat, method = dist_fun, p = p,
                              test.na = FALSE, unit = unit, est.prob = NULL)
  dimnames(mat) = list(x$id, x$id)
  stats::as.dist(mat)
}
