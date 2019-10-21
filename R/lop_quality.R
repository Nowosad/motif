# library(comat)
# library(raster)
# library(dplyr)
# library(sf)
# landcover = raster(system.file("raster/landcover.tif", package = "lopata"))
# # plot(landcover)
# lc_coma = lop_coma(landcover, size = 100)
# lc_grid = lop_grid(landcover, size = 100) %>%
#   inner_join(lc_coma[c("row", "col")], by = c("row", "col"))
# lc_cove = lop_cove(lc_coma, normalization = "pdf", ordered = FALSE)
# lc_dist = lop_to_dist(lc_cove, method = "jensen-shannon")
# lc_hclust = hclust(lc_dist, method = "ward.D2")
# lc_ids = cutree(lc_hclust, k = 12)
#
# lc_clusters = lop_clusters(lc_grid, lc_ids)
# lc_clusters$iso = lop_isolation(lc_clusters, lc_dist)
# lc_clusters$inh = lop_inhomogeneity(lc_clusters, lc_dist)
#
# lc_segments = lop_segments(lc_grid, lc_ids)
# lc_segments$iso = lop_isolation(lc_segments, lc_dist)
# lc_segments$inh = lop_inhomogeneity(lc_segments, lc_dist)
#
# x_dist = lc_dist
# x = lc_clusters
#
# x_xclu$iso = lop_isolation(x, cove_dist)
# x_clu$inh = lop_inhomogeneity(x, cove_dist)
# x_seg$iso = lop_isolation(x_seg, cove_dist)
# x_seg$inh = lop_inhomogeneity(x_seg, cove_dist)

# write_sf(x_clu, "x_clu.gpkg")
# write_sf(x_seg, "x_seg.gpkg")
#
#
# mean(lop_isolation(x_clu, cove_dist))
# mean(lop_inhomogeneity(x_clu, cove_dist))
# mean(lop_isolation(x_seg, cove_dist))
# mean(lop_inhomogeneity(x_seg, cove_dist))
#' lop_isolation
#' @export
lop_isolation = function(x, x_dist){
  x_dist = as.matrix(x_dist)

  x_grid_neigh = spdep::poly2nb(x, queen = TRUE)

  unique_clust = seq_len(nrow(x))

  vapply(unique_clust, get_isolation, x = x, x_dist = x_dist,
         x_grid_neigh = x_grid_neigh, FUN.VALUE = 1.0)

}

get_isolation = function(x_clust, x, x_dist, x_grid_neigh){
  # print(x_clust)
  x_clust_neigh = x_grid_neigh[[x_clust]]
  if (sum(x_clust_neigh) == 0){
    return(NA)
  } else {
    x_clust_dist = vector(mode = "numeric", length = length(x_clust_neigh))
    for (i in seq_along(x_clust_neigh)){
      # print(i)
      x_clust_dist[i] = mean(x_dist[x[["data"]][(x[["clust"]] == x_clust)][[1]][["motifels"]],
                                    x[["data"]][(x[["clust"]] == x_clust_neigh[i])][[1]][["motifels"]]])
    }
    return(mean(x_clust_dist))
  }
}
#' lop_inhomogeneity
#' @export
lop_inhomogeneity = function(x, x_dist){
  x_dist = as.matrix(x_dist)

  unique_clust = seq_len(nrow(x))

  vapply(unique_clust, get_inhomogeneity, x = x, x_dist = x_dist, FUN.VALUE = 1.0)
}

get_inhomogeneity = function(x, x_dist, unique_clust){
  x_clust_dist = x_dist[x[["data"]][(x[["clust"]] == unique_clust)][[1]][["motifels"]],
                        x[["data"]][(x[["clust"]] == unique_clust)][[1]][["motifels"]]]

  if (sum(x_clust_dist) != 0){
    x_clust_dist = as.dist(x_clust_dist)
    return(mean(x_clust_dist))
  } else {
    return(0)
  }
}
