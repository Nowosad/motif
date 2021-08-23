#' Transforms lsp objects
#'
#' It allows for transforming spatial signatures (outputs of
#' the `lsp_signature()` function) using user-provided functions.
#' See examples for more details.
#'
#' @param x Object of class `lsp` - usually the output of
#' the `lsp_signature()` function.
#' @param fun A user-provided function.
#' @param ... Additional arguments for `fun`.
#'
#' @return Object of class `lsp`.
#' It has three columns: (1) `id` - an id of each window.
#' For irregular windows, it is the values provided in the `window` argument,
#' (2) `na_prop` - share (0-1) of `NA` cells for each window,
#' (3) `signature` - a list-column containing with calculated signatures
#' @export
#'
#' @examples
#' library(stars)
#' landform = read_stars(system.file("raster/landforms.tif", package = "motif"))
#' result_coma500 = lsp_signature(landform, type = "coma", threshold = 0.5, window = 500)
#'
#' #see how the first signature looks
#' result_coma500$signature[[1]]
#'
#' my_function = function(mat){
#'     mat_c = colSums(mat)
#'     freqs = mat_c / sum(mat)
#'     # entropy
#'     -sum(freqs * log2(freqs), na.rm = TRUE)
#' }
#'
#' result_coma500_2 = lsp_transform(result_coma500, my_function)
#'
#' #see how the first signature looks after transformation
#' result_coma500_2$signature[[1]]
#'
#' \donttest{
#' # larger data example
#' library(stars)
#' landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' result_coma500 = lsp_signature(landform, type = "coma", threshold = 0.5, window = 500)
#'
#' #see how the first signature looks
#' result_coma500$signature[[1]]
#'
#' my_function = function(mat){
#'     mat_c = colSums(mat)
#'     freqs = mat_c / sum(mat)
#'     # entropy
#'     -sum(freqs * log2(freqs), na.rm = TRUE)
#' }
#'
#' result_coma500_2 = lsp_transform(result_coma500, my_function)
#'
#' #see how the first signature looks after transformation
#' result_coma500_2$signature[[1]]
#' }
lsp_transform = function(x, fun, ...){
  x$signature = lapply(x$signature, fun, ...)
  return(x)
}
