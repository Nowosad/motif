#' Extracts a local landscape
#'
#' Extracts a local landscape from categorical raster data based on its id and provided `window` argument.
#'
#' @param x Object of class `stars`, `stars_proxy`, or terra's `SpatRaster`.
#' @param window Specifies areas for analysis. It can be either: `NULL`, a numeric value, or an `sf` object.
#' @param id Id of the local landscape - it is possible to find in the output of `lsp_signature()`, `lsp_search()`, `lsp_compare()`, or `lsp_add_clusters()`.
#'
#' @return A `stars`or `terra` object cropped to the extent of a selected local landscape
#'
#' @export
#'
#' @examples
#' library(stars)
#' landform = read_stars(system.file("raster/landforms.tif", package = "motif"))
#' ecoregions = read_sf(system.file("vector/ecoregionss.gpkg", package = "motif"))
#'
#' extract1 = lsp_extract(x = landform, window = 100, id = 25)
#' plot(extract1)
#'
#' ecoregions = st_transform(ecoregions, st_crs(landform))
#' extract2 = lsp_extract(x = landform, window = ecoregions["id"], id = 11)
#' plot(extract2)
#'
#' \donttest{
#' # larger data example
#' library(stars)
#' landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
#'
#' extract1 = lsp_extract(x = landform, window = 100, id = 1895)
#' #plot(extract1)
#'
#' ecoregions = st_transform(ecoregions, st_crs(landform))
#' extract2 = lsp_extract(x = landform, window = ecoregions["id"], id = 7)
#' #plot(extract2)
#' }
lsp_extract = function(x, window, id){
  windows_sf = lsp_add_sf(x = x, window = window)
  windows_sf = windows_sf[windows_sf$id == id, ]
  if (inherits(x, "stars")){
    output_stars = stars::st_as_stars(x[windows_sf])
    return(output_stars)
  } else if (inherits(x, "SpatRaster")){
    output_terra = terra::crop(x, windows_sf)
    return(output_terra)
  }
}
