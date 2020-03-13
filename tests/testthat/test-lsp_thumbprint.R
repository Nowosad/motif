context("thumbprint")


# landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
# plot(landcover)
landform = read_stars(system.file("raster/landform.tif", package = "motif"))
# plot(landform)
landform_p = read_stars(system.file("raster/landform.tif", package = "motif"), proxy = TRUE)
# plot(landform_p)
# ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
# plot(ecoregions["id"])
# set.seed(222)
# random_ndvi = landcover
# random_ndvi$ndvi = sample(x = 1:10, size = length(random_ndvi[[1]]), replace = TRUE)
# random_ndvi$landcover2015.tif = NULL
# plot(random_ndvi)
# landcover_p = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = TRUE)

test_that("thumprint works corectly for whole area", {
  expect_equal(lsp_thumbprint(landform, type = "coma", threshold = 1)$signature[[1]],
               comat::get_coma(landform$landform.tif))
})

test_that("stars results are equal to stars.proxy results", {
  expect_equal(lsp_thumbprint(landform, type = "coma",
                              threshold = 0.5, window_size = 500)$signature[[1]],
               lsp_thumbprint(landform_p, type = "coma",
                              threshold = 0.5, window_size = 500)$signature[[1]])
})

# # check stars.proxy -------------------------------------------------------
#
#
# # check stars.proxy -------------------------------------------------------
# expect_equal(lsp_thumbprint(landform, type = "coma", threshold = 1)$signature[[1]],
#              lsp_thumbprint(landform_p, type = "coma", threshold = 1)$signature[[1]])
#
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
# lsp_thumbprint(landcover, type = "coma", window = ecoregions["id"], threshold = 0.9)
#
# # proxy motifel1 ----------------------------------------------------------
# lsp_thumbprint(landcover_p, type = "coma", threshold = 0.9)
#
# # proxy motifel2 ----------------------------------------------------------
# lsp_thumbprint(landcover_p, type = "coma", threshold = 0.9, window_size = 100, window_shift = 100)
#
# # proxy motifel3 ----------------------------------------------------------
# lsp_thumbprint(landcover_p, type = "cove", threshold = 0.9, window_size = 100, window_shift = 100)
#
# # proxy window1 -----------------------------------------------------------
# q = lsp_thumbprint(landcover_p, type = "coma", window = ecoregions[1, "id"], threshold = 0.9)
#
# # all.equal(
# #   lsp_thumbprint(landcover, type = "coma", window = ecoregions["id"], threshold = 0.9),
# #   lsp_thumbprint(landcover_p, type = "coma", window = ecoregions["id"], threshold = 0.9)
# # )
#
# # proxy window2 -----------------------------------------------------------
# a = lsp_thumbprint(landcover_p, type = "cove", window = ecoregions[1, "id"], threshold = 0.9, normalization = "none")
