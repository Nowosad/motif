# library(stars)
# library(rcartocolor)
#
# landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
# # plt(landcover)
# lc_cove = lsp_thumbprint(landcover, type = "cove", window_size = 200, normalization = "pdf")
# lc_dist = lsp_to_dist(lc_cove, dist_fun = "jensen-shannon")
# lc_hclust = hclust(lc_dist, method = "ward.D2")
# clusters = cutree(lc_hclust, k = 12)
#
# # stars -------------------------------------------------------------------
# lc_grid_stars = lsp_add_clusters(lc_cove, clusters)
# plot(lc_grid_stars["clust"], col = carto_pal(12, "Safe"))
#
# # sf ----------------------------------------------------------------------
# lc_grid_sf = lsp_add_clusters(lc_cove, clusters, output = "sf")
#
# safe_pal = function(n){
#   carto_pal(n, "Safe")
# }
#
# lc_grid_sf$clust = as.factor(lc_grid_sf$clust)
# plot(lc_grid_sf["clust"], pal = safe_pal)
#
# # mapview::mapview(lc_grid_sf["clust"])
