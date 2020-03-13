# library(stars)
#
# lc = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
# lf = read_stars(system.file("raster/landform.tif", package = "motif"))
# lcp = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = TRUE)
# lfp = read_stars(system.file("raster/landform.tif", package = "motif"), proxy = TRUE)
#
# set.seed(222)
# rn = lc
# rn$ndvi = sample(x = 1:10, size = length(rn[[1]]), replace = TRUE)
# rn$landcover2015.tif = NULL
#
# ext = st_bbox(c(xmin = -249797.344531127, xmax = -211162.693944285,
#                 ymin = -597280.143035389, ymax = -558645.492448547),
#                 crs = st_crs(lc))
#
# ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
# plot(ecoregions["id"])
#
# lc_ext = lc[ext]
# plot(lc_ext)
# lf_ext = lf[ext]
# plot(lf_ext)
# rn_ext = rn[ext]
# plot(rn_ext)
#
# # all area ----------------------------------------------------------------
# s1 = lsp_search(lc_ext, lc, type = "cove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # cocove ------------------------------------------------------------------
# s2 = lsp_search(c(lc_ext, lf_ext), c(lc, lf), type = "cocove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # wecove ------------------------------------------------------------------
# s3 = lsp_search(c(lc_ext, rn_ext), c(lc, rn), type = "wecove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # incove ------------------------------------------------------------------
# s4 = lsp_search(c(lc_ext, lf_ext), c(lc, lf), type = "incove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # motifel -----------------------------------------------------------------
# s5 = lsp_search(lf_ext, lf, type = "cove",
#   dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250)
#
# # window ------------------------------------------------------------------
# s6 = lsp_search(lc_ext, lc, type = "cove",
#   dist_fun = "jensen-shannon", threshold = 1, window = ecoregions["id"])
#
# # motifel proxy -----------------------------------------------------------
# s7 = lsp_search(lf_ext, lfp, type = "cove",
#                 dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250)
#
# # window proxy ------------------------------------------------------------
# s8 = lsp_search(lc_ext, lcp, type = "cove",
#                 dist_fun = "jensen-shannon", threshold = 1, window = ecoregions["id"])
#
# # all.equal(s6, s8)
# # bench::mark(lsp_search(lf_ext, lf, type = "cove",
# #                        dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250),
# #             lsp_search(lf_ext, lfp, type = "cove",
# #                        dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250))
