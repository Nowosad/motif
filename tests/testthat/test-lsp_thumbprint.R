# library(stars)
# library(motif)
# landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
# plot(landcover)
# landform = read_stars(system.file("raster/landform.tif", package = "motif"))
# plot(landform)
# ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
# plot(ecoregions["id"])
# set.seed(222)
# random_ndvi = landcover
# random_ndvi$ndvi = sample(x = 1:10, size = length(random_ndvi[[1]]), replace = TRUE)
# random_ndvi$landcover2015.tif = NULL
# plot(random_ndvi)
# landcover_p = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = TRUE)
#
# # all area ----------------------------------------------------------------
# lsp_thumbprint(landform, type = "coma", threshold = 0.9)
#
# # motifel -----------------------------------------------------------------
# lsp_thumbprint(landform, type = "coma", threshold = 0.9, window_size = 100, window_shift = 100)
#
# # cove --------------------------------------------------------------------
# lsp_thumbprint(landform, type = "cove", threshold = 0.9)
#
# # one class only ----------------------------------------------------------
# lsp_thumbprint(landform, type = "cove", threshold = 0.9, classes = 10)
#
# # cocove ------------------------------------------------------------------
# lsp_thumbprint(c(landcover, landform), type = "cocove", threshold = 0.9)
#
# # wecove ------------------------------------------------------------------
# lsp_thumbprint(c(landcover, random_ndvi), type = "wecove", threshold = 0.9)
#
# # incove ------------------------------------------------------------------
# lsp_thumbprint(c(landcover, landform), type = "incove", threshold = 0.9)
#
# # window ------------------------------------------------------------------
# b = lsp_thumbprint(landcover, type = "coma", window = ecoregions["id"], threshold = 0.9)
#
# # proxy motifel -----------------------------------------------------------
# lsp_thumbprint(landcover_p, type = "coma", threshold = 0.9, window_size = 100, window_shift = 100)
#
# # proxy window ------------------------------------------------------------
# a = lsp_thumbprint(landcover_p, type = "coma", window = ecoregions["id"], threshold = 0.9)
#
