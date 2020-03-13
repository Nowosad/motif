# library(comat)
# library(stars)
# data(raster_x, package = "comat")
# raster_x = st_as_stars(raster_x)
# plot(raster_x)
#
# cov = lsp_thumbprint(raster_x, window_size = 2, type = "cove")
#
# dist_cov = lsp_to_dist(cov, dist_fun = "jensen-shannon")
# dist_cov
