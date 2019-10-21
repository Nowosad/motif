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
# x = lc_grid
# ids = lc_ids


# x = lop_grid(lc, size = 100)
# ids = cutree(coma_hclust, k = 12)
# x_clu = lop_clusters(x, ids)
# x_seg = lop_segments(x, ids)

#' lop_clusters
#' @export
lop_clusters = function(x, ids){
  x[["clust"]] = ids
  x[["motifels"]] = seq_len(nrow(x))
  x_sum = dplyr::summarise(dplyr::group_by(x, clust))
  x_sum = sf::st_collection_extract(x_sum, "POLYGON")

  df_sum = tidyr::nest(st_drop_geometry(x[c("clust", "motifels")]), data = c(motifels))
  dplyr::left_join(x_sum, df_sum, by = "clust")
}
#' lop_segments
#' @export
lop_segments = function(x, ids){
  x[["clust"]] = ids
  x[["motifels"]] = seq_len(nrow(x))
  x_sum = dplyr::summarise(dplyr::group_by(x, clust))
  x_sum = sf::st_collection_extract(x_sum, "POLYGON")
  x[["clust"]] = NULL

  x_seg = st_cast(x_sum, "POLYGON")
  x_seg$clust = seq_len(nrow(x_seg))

  x2 = st_join(x, x_seg, largest = TRUE)

  df_sum = tidyr::nest(st_drop_geometry(x2[c("clust", "motifels")]), data = c(motifels))
  dplyr::left_join(x_seg, df_sum, by = "clust")
}


