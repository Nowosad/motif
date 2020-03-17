context("add spatial")

landform_lsp_sf = lsp_add_sf(landform, window_size = 100)

test_that("tests landform_lsp_sf works on default", {
  expect_equal(dim(landform_lsp_sf), c(2886, 2))
})
