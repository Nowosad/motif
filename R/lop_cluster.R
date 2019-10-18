# x = lop_grid(lc, size = 100)
# ids = cutree(coma_hclust, k = 12)
#' lop_clusters
#' @export
lop_clusters = function(x, ids){
  x[["clust"]] = ids
  x[["motifels"]] = seq_len(nrow(x))
  x_sum = dplyr::summarise(dplyr::group_by(x, clust))
  x_sum = sf::st_collection_extract(x_sum, "POLYGON")

  df_sum = tidyr::nest(st_drop_geometry(x), data = c(motifels))
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

  df_sum = tidyr::nest(st_drop_geometry(x2), data = c(motifels))
  dplyr::left_join(x_seg, df_sum, by = "clust")
}

# x_clu = lop_clusters(x, ids)
# x_seg = lop_segments(x, ids)
