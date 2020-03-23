context("add spatial")

landform_lsp_sf = lsp_add_sf(landform, window = 100)
landform_lsp_sf2 = lsp_add_sf(landform)
landform_lsp_sf3 = lsp_add_sf(landform, window = ecoregions["id"])

test_that("tests landform_lsp_sf works on default", {
  expect_equal(dim(landform_lsp_sf), c(2886, 2))
  expect_equal(dim(landform_lsp_sf2), c(1, 2))
  expect_equal(dim(landform_lsp_sf3), c(22, 2))
})
