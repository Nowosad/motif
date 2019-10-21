# library(lopata)
# library(comat)
# library(raster)
# library(dplyr)
# library(sf)
# landcover = raster(system.file("raster/landcover.tif", package = "lopata"))
# ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "lopata"))
#
# # plot(landcover)
# lc_coma = lop_coma(landcover, size = 100)
# lc_grid = lop_grid(landcover, size = 100) %>%
#   inner_join(lc_coma[c("row", "col")], by = c("row", "col"))
#
# x = ecoregions
# x$id = NULL
# y = lc_grid
# y$id = seq_len(nrow(y))
#' lop_add_motifels
#' @export
lop_add_motifels = function(x, y){
  y$id = seq_len(nrow(y))
  x$clust = seq_len(nrow(x))
  names(y)[names(y) == "id"] = "motifels"
  x2 = st_join(y, x, largest = TRUE)
  x3 = x2 %>% group_by(clust) %>% summarise()
  x4 = sf::st_collection_extract(x3, "POLYGON")

  df_sum = tidyr::nest(st_drop_geometry(x2[c("clust", "motifels")]), data = c(motifels))
  x5 = dplyr::left_join(x, df_sum, by = "clust")
  x5
}
