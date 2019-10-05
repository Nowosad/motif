#' Title
#'
#' @param x
#' @param method [philentropy::getDistMethods()]
#' @param unit
#' @param p
#'
#' @return
#'
#' @aliases to_dist
#' @rdname to_dist
#'
#' @examples
#'
#' @export
to_dist = function(x, method, unit, p) UseMethod("to_dist")
#'
#' @name to_dist
#' @export
to_dist.cove = function(x, method, unit = "log", p = NULL){
  vec_to_dist(vec = x$vector, method = method, unit = unit, p = p)
}
#' @name to_dist
#' @export
to_dist.cocove = function(x, method, unit = "log", p = NULL){
  vec_to_dist(vec = x$vector, method = method, unit = unit, p = p)
}
#' @name to_dist
#' @export
to_dist.wecove = function(x, method, unit = "log", p = NULL){
  vec_to_dist(vec = x$vector, method = method, unit = unit, p = p)
}
#' @name to_dist
#' @export
to_dist.incoma = function(x, method, unit = "log", p = NULL){
  vec_to_dist(vec = x$vector, method = method, unit = unit, p = p)
}

vec_to_dist = function(vec, method, unit, p){
  mat = do.call(rbind, vec)
  mat = philentropy::distance(mat, method = method, p = p, test.na = FALSE, unit = unit)
  stats::as.dist(mat)
}
