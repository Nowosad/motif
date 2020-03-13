# library(stars)
# landform = read_stars(system.file("raster/landform.tif", package = "motif"))
# plot(landform)
#
# lc_cove = lsp_thumbprint(landform, type = "cove", window_size = 100, normalization = "pdf")
# lc_cove
#
# # stars1 ------------------------------------------------------------------
# landform_lsp_stars = lsp_add_stars(landform, window_size = 100)
# plot(landform_lsp_stars)
#
# # stars2 ------------------------------------------------------------------
# lc_cove_lsp_stars = lsp_add_stars(lc_cove)
#
# plot(lc_cove_lsp_stars)
# plot(lc_cove_lsp_stars["na_prop"])
#
# # sf1 ---------------------------------------------------------------------
# landform_lsp_sf = lsp_add_sf(landform, window_size = 100)
# plot(landform_lsp_sf)
#
# # sf2 ---------------------------------------------------------------------
# lc_cove_lsp_sf = lsp_add_sf(lc_cove)
#
# plot(lc_cove_lsp_sf["id"])
# plot(lc_cove_lsp_sf["na_prop"])
#
