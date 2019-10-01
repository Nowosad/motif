#' Create a co-occurrence vector (wecove)
#'
#' Converts a co-occurrence matrix (wecoma) to
#' a co-occurrence vector (wecove)
#'
#' @param x A matrix - an output of the [get_coma()] function
#' @param ordered The type of pairs considered.
#' Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#'
#' @return A co-occurrence vector
#' @export
#'
#' @examples
#' library(comat)
#' library(raster)
#' data(x, package = "comat")
#'
#' com = get_coma2(x)
#' com
#'
#' cov = get_cove(com$matrix[[1]])
#' cov
#'
#' get_cove2(com)
get_cove = function(x, ordered = TRUE){
  # x = raster::as.matrix(x)
  y = rcpp_get_vec(x, ordered)
  structure(y, class = c("numeric", "cove"))
}

#' @export
get_cove2 = function(x, ordered = TRUE, normalization = NULL){
  y = lapply(x$matrix, rcpp_get_vec, ordered = ordered)
  x$matrix = NULL
  if(missing(normalization)){
    y
  } else{
    y = lapply(y, get_normalized, normalization = normalization)
  }
  x$vector = y
  return(x)
  # structure(y, class = c("numeric", "cove"))
}
