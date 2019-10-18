# x_dist = cove_dist
# x = x_clu
#
# x_clu$iso = lop_isolation(x_clu, cove_dist)
# x_clu$inh = lop_inhomogeneity(x_clu, cove_dist)
# x_seg$iso = lop_isolation(x_seg, cove_dist)
# x_seg$inh = lop_inhomogeneity(x_seg, cove_dist)
#
# write_sf(x_clu, "x_clu.gpkg")
# write_sf(x_seg, "x_seg.gpkg")
#
#
# mean(lop_isolation(x_clu, cove_dist))
# mean(lop_inhomogeneity(x_clu, cove_dist))
# mean(lop_isolation(x_seg, cove_dist))
# mean(lop_inhomogeneity(x_seg, cove_dist))
#' @export
lop_isolation = function(x, x_dist){
  x_dist = as.matrix(x_dist)

  x_grid_neigh = poly2nb(x, queen = TRUE)

  unique_clust = seq_len(nrow(x))

  vapply(unique_clust, get_isolation, x = x, x_dist = x_dist,
         x_grid_neigh = x_grid_neigh, FUN.VALUE = 1.0)

}

get_isolation = function(x_clust, x, x_dist, x_grid_neigh){
  x_clust_neigh = x_grid_neigh[[x_clust]]
  x_clust_dist = vector(mode = "numeric", length = length(x_clust_neigh))
  for (i in seq_along(x_clust_neigh)){
    x_clust_dist[i] = mean(x_dist[x[["data"]][(x[[x_clust]] == unique_clust)][[1]][["motifels"]],
                                  x[["data"]][(x[[x_clust]] == x_clust_neigh[i])][[1]][["motifels"]]])
  }
  mean(x_clust_dist)
}

#' @export
lop_inhomogeneity = function(x, x_dist){
  x_dist = as.matrix(x_dist)

  unique_clust = seq_len(nrow(x))

  vapply(unique_clust, get_inhomogeneity, x = x, x_dist = x_dist, clust = clust, FUN.VALUE = 1.0)
}

get_inhomogeneity = function(x, x_dist, clust, unique_clust){
  x_dist = x_dist[x[["data"]][(x[[clust]] == unique_clust)][[1]][["motifels"]],
                  x[["data"]][(x[[clust]] == unique_clust)][[1]][["motifels"]]]

  if (sum(x_dist) != 0){
    x_dist = as.dist(x_dist)
    return(mean(x_dist))
  } else {
    return(0)
  }
}
