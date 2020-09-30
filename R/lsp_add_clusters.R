#' Adds clusters' ids to a lsp object
#'
#' Adds clusters' ids to a lsp object.
#' The output can be of `stars` or `sf` class.
#' See examples.
#'
#' @param x Object of class `lsp` - usually the output of
#' the `lsp_signature()` function
#' @param clust Vector containing an id value for each row in `x`
#' @param output The class of the output. Either `"stars"` or `"sf"`
#' @param window Specifies areas for analysis. It can be either: `NULL` or an `sf` object. If `window=NULL` calculations are performed based on the metadata from `x`. If an `sf` object is provided, each feature (row) defines the extent of a local pattern. The `sf` object should have one attribute (otherwise, the first attribute is used as an id).
#'
#' @examples
#' \dontrun{
#' library(stars)
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
#' safe_pal = c("#88CCEE", "#CC6677", "#DDCC77",
#'              "#117733", "#332288", "#888888")
#'
#' landform_grid_stars = lsp_add_clusters(landform_cove, clusters)
#' plot(landform_grid_stars["clust"], col = safe_pal)
#'
#' landform_grid_starsq = lsp_add_quality(landform_grid_stars,
#'                                        landform_dist)
#' plot(landform_grid_starsq["quality"])
#' }
#' @export
lsp_add_clusters = function(x, clust, output = "sf", window = NULL){
  x$clust = clust
  if (output == "stars"){
    x = lsp_add_stars(x, window = window)
  } else if (output == "sf"){
    x = lsp_add_sf(x, window = window)
  }
  return(x)
}
