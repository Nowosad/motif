#' Extracts a local landscape
#'
#' Extracts a local landscape from categorical raster data based on its id and provided `window` argument.
#'
#' @param x Object of class `stars` or `stars_proxy`.
#' @param window Specifies areas for analysis. It can be either: `NULL`, a numeric value, or an `sf` object.
#' @param id Id of the local landscape - it is possible to find in the output of `lsp_signature()`, `lsp_search()`, `lsp_compare()`, or `lsp_add_clusters()`.
#'
#' @export
#'
#' @examples
#' library(stars)
#' landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
#'
#' extract1 = lsp_extract(x = landform, window = 100, id = 1895)
#' plot(extract1)
#'
#' extract2 = lsp_extract(x = landform, window = ecoregions["id"], id = 7)
#' plot(extract2)
lsp_extract = function(x, window, id){
  windows_sf = lsp_add_sf(x = x, window = window)
  windows_sf = windows_sf[windows_sf$id == id, ]
  x[windows_sf]
}
