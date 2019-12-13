#' @param x
#'
#' @param clust
#'
#' @examples
#' library(comat)
#' library(stars)
#' library(dplyr)
#' library(rcartocolor)
#' library(sf)
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
#' lc_grid = lsp_add_quality(lc_grid, lc_dist, "clust")
#' plot(lc_grid["inhomogeneity"])
#' plot(lc_grid["isolation"])
#' plot(lc_grid["quality"])
#'
#' @export
lsp_add_clusters = function(x, clust){
  x$clust = clust
  x = lsp_add_spatial(x)
  return(x)
}
