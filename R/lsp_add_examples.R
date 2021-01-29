#' Adds spatial data of each region in an lsp or sf object
#'
#' Adds spatial data of each region in an lsp or sf object.
#' The output is an lsp or sf object with an additional column `"region"`.
#' See examples.
#'
#' @param x Object of class `lsp` - usually a subset of the output of `lsp_signature()`
#' or an object of class `sf` - usually a subset of the output of `lsp_search()`
#' @param y Object of class `stars` or `stars_proxy`.
#' @param window Specifies areas for analysis. It can be either: `NULL` or an `sf` object.
#' The `sf` object is only needed for adding examples of irregular regions.
#'
#' @return The input object with a new column `"region"`.
#' The `"region"` column is a list with a raster extracted for each row.
#'
#' @examples
#'
#' library(stars)
#'
#' landcover = read_stars(system.file("raster/landcover2015s.tif", package = "motif"))
#'
#' landcover_coma = lsp_signature(landcover, type = "coma", threshold = 0.9, window = 100)
#' selected_coma = subset(landcover_coma, id %in% c(5, 10, 15, 35))
#' selected_coma
#'
#' selected_coma = lsp_add_examples(x = selected_coma, y = landcover)
#' selected_coma
#'
#' plot(selected_coma$region[[1]])
#' plot(selected_coma$region[[4]])
#'
#' \donttest{
#' # larger data example
#' library(stars)
#'
#' landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#'
#' landcover_coma = lsp_signature(landcover, type = "coma", threshold = 0.9, window = 100)
#' selected_coma = subset(landcover_coma, id %in% c(5, 80, 1971, 2048))
#' selected_coma
#'
#' selected_coma = lsp_add_examples(x = selected_coma, y = landcover)
#' selected_coma
#'
#' plot(selected_coma$region[[1]])
#' plot(selected_coma$region[[4]])
#' }
#' @aliases lsp_add_examples
#' @rdname lsp_add_examples
#'
#' @export
lsp_add_examples = function(x, y, window = NULL) UseMethod("lsp_add_examples")

#' @name lsp_add_examples
#' @export
lsp_add_examples.lsp = function(x, y, window = NULL){

  windows_sf = lsp_add_sf(x = x, window = window)

  x$region = vector(mode = "list", length = nrow(x))
  for (i in seq_len(nrow(x))){
    windows_sf_id = windows_sf[windows_sf$id == x$id[[i]], ]
    if (is.null(window)){
      x$region[[i]] = stars::st_as_stars(y[sf::st_bbox(windows_sf_id)])
    } else {
      x$region[[i]] = stars::st_as_stars(y[windows_sf_id])
    }
  }
  x
}

#' @name lsp_add_examples
#' @export
lsp_add_examples.sf = function(x, y, window = NULL){
  if (!inherits(x, "tbl_df")){
    x = sf::st_as_sf(tibble::as_tibble(x))
  }

  windows_sf = x
  x$region = vector(mode = "list", length = nrow(x))
  for (i in seq_len(nrow(x))){
    windows_sf_id = windows_sf[windows_sf$id == x$id[[i]], ]
    x$region[[i]] = stars::st_as_stars(y[windows_sf_id])
  }
  x
}
