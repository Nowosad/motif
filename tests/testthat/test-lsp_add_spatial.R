context("add spatial")

landform_lsp_stars = lsp_add_stars(st_dimensions(landform), window = 200)
landform_lsp_sf = lsp_add_sf(landform, window = 100)
landform_lsp_sf2 = lsp_add_sf(landform)
landform_lsp_sf3 = lsp_add_sf(landform, window = ecoregions["id"])

test_that("tests landform_lsp_sf works on default", {
  expect_equal(dim(landform_lsp_sf), c(49, 2))
  expect_equal(dim(landform_lsp_sf2), c(1, 2))
  expect_equal(dim(landform_lsp_sf3), c(5, 2))
  expect_equal(st_crs(landform_lsp_stars), st_crs(landform))
})

result_coma = lsp_signature(landform, type = "cove", threshold = 1, window = 200)
landform_lsp_stars2 = lsp_add_stars(result_coma)
landform_lsp_sf4 = lsp_add_sf(result_coma)
landform_lsp_stars3 = lsp_add_stars(result_coma, metadata = FALSE)
landform_lsp_sf5 = lsp_add_sf(result_coma, metadata = FALSE)

test_that("tests lsp_add_spatial works on lsp", {
  expect_equal(length(landform_lsp_stars2), 80)
  expect_equal(length(landform_lsp_stars3), 78)
  expect_equal(ncol(landform_lsp_sf4), 4)
  expect_equal(ncol(landform_lsp_sf5), 2)
})
