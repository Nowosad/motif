#' Changes structure of the lsp object
#'
#' Converts a list-column with signatures into many numeric columns
#'
#' @param x - an lsp object
#'
#' @return Object of class `lsp`.
#' It has several columns: (1) `id` - an id of each window.
#' For irregular windows, it is the values provided in the `window` argument,
#' (2) `na_prop` - share (0-1) of `NA` cells for each window,
#' (3) one or more columns representing values of the signature
#'
#' @export
#' @examples
#'
#' library(stars)
#'
#' landcover = read_stars(system.file("raster/landcover2015s.tif", package = "motif"))
#'
#' landcover_cove = lsp_signature(landcover, type = "cove", threshold = 0.9, window = 100)
#' landcover_cover = lsp_restructure(landcover_cove)
#' landcover_cover
#'
#' lsp_add_sf(landcover_cover)
lsp_restructure = function(x){
  x_attr = attributes(x)
  nc = ncol(x$signature[[1]])
  nr = nrow(x$signature[[1]])
  if (nr > 1){
    stop(paste0("lsp_restructure() only works for one dimensional signatures (e.g., 'cove')",
                "and not for two dimensional ones (e.g., 'coma'"), call. = FALSE)
  }

  unnested_signature = matrix(unlist(x$signature, use.names = FALSE),
                              ncol = nc, byrow = TRUE)
  colnames(unnested_signature) = paste0("X", seq_len(nc))
  unnested_signature = tibble::as_tibble(unnested_signature)

  x["signature"] = NULL

  x = tibble::as_tibble(cbind(x, unnested_signature))
  x_attr$names = names(x)
  attributes(x) = x_attr

  x
}
