#' Title
#'
#' @param x
#'
#' @param clust
#'
#' @examples
#' library(stars)
#' library(rcartocolor)
#'
#' landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#' # plot(landcover)
#' lc_cove = lsp_thumbprint(landcover, type = "cove", window_size = 100, normalization = "pdf")
#' lc_dist = lsp_to_dist(lc_cove, dist_fun = "jensen-shannon")
#' lc_hclust = hclust(lc_dist, method = "ward.D2")
#' clusters = cutree(lc_hclust, k = 12)
#'
#' lc_grid = lsp_add_clusters(lc_cove, clusters)
#' plot(lc_grid["clust"], col = carto_pal(12, "Safe"))
#'
#' @export
lsp_add_clusters = function(x, clust){
  x$clust = clust
  x = lsp_add_spatial(x)
  return(x)
}
