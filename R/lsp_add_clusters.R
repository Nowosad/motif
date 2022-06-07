#' Adds clusters' ids to a lsp object
#'
#' Adds clusters' ids to a lsp object.
#' The output can be of `stars`, `sf`, or `terra` class.
#' See examples.
#'
#' @param x Object of class `lsp` - usually the output of
#' the `lsp_signature()` function
#' @param clust Vector containing an id value for each row in `x`
#' @param output The class of the output. Either `stars`, `sf`, or `terra`
#' @param window Specifies areas for analysis. It can be either: `NULL` or an `sf` object. If `window=NULL` calculations are performed based on the metadata from `x`. If an `sf` object is provided, each feature (row) defines the extent of a local pattern. The `sf` object should have one attribute (otherwise, the first attribute is used as an id).
#' @return Object of class `stars`, `sf`, or `terra` (depending on the `output` argument) with an additional column `"clust"` representing clusters' id values.
#'
#' @examples
#'
#' library(stars)
#' library(sf)
#' landform = read_stars(system.file("raster/landforms.tif", package = "motif"))
#' landform_cove = lsp_signature(landform,
#'                                type = "cove",
#'                                window = 200,
#'                                normalization = "pdf")
#'
#' landform_dist = lsp_to_dist(landform_cove,
#'                             dist_fun = "jensen-shannon")
#'
#' landform_hclust = hclust(landform_dist, method = "ward.D2")
#' #plot(landform_hclust)
#'
#' clusters = cutree(landform_hclust, k = 4)
#'
#' landform_grid_sf = lsp_add_clusters(landform_cove, clusters)
#' #plot(landform_grid_sf["clust"])
#'
#' #landform_grid_sfq = lsp_add_quality(landform_grid_sf,
#' #                                        landform_dist)
#' #plot(landform_grid_sfq["quality"])
#'
#' \donttest{
#' # larger data example
#' library(stars)
#' library(sf)
#' landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' landform_cove = lsp_signature(landform,
#'                                type = "cove",
#'                                window = 200,
#'                                normalization = "pdf")
#'
#' landform_dist = lsp_to_dist(landform_cove,
#'                             dist_fun = "jensen-shannon")
#'
#' landform_hclust = hclust(landform_dist, method = "ward.D2")
#' plot(landform_hclust)
#'
#' clusters = cutree(landform_hclust, k = 6)
#'
#' landform_grid_sf = lsp_add_clusters(landform_cove, clusters)
#' plot(landform_grid_sf["clust"])
#'
#' landform_grid_sfq = lsp_add_quality(landform_grid_sf,
#'                                        landform_dist)
#' plot(landform_grid_sfq["quality"])
#' }
#' @export
lsp_add_clusters = function(x, clust, output = "sf", window = NULL){
  x$clust = clust
  if (output == "stars"){
    x = lsp_add_stars(x, window = window)
  } else if (output == "sf"){
    x = lsp_add_sf(x, window = window)
  } else if (output == "terra"){
    x = lsp_add_terra(x, window = window)
  }
  return(x)
}
