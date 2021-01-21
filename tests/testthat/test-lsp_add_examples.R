context("add examples")

result_coma500 = lsp_signature(landform, type = "coma",
                               threshold = 1, window = 500)
selected_coma = lsp_add_examples(x = result_coma500, y = landform)
selected_coma

result_compositionwindow = lsp_signature(landform, type = "composition",
                                         threshold = 1, window = ecoregions["id"])

result_compositionwindow2 = subset(result_compositionwindow, id == 17)
selected_coma2 = lsp_add_examples(x = result_compositionwindow2,
                                  y = landform,
                                  window = ecoregions["id"])
selected_coma2

test_that("tests landform_lsp_sf works on default", {
  expect_equal(dim(selected_coma), c(4, 4))
  expect_s3_class(selected_coma$region[[1]], "stars")
  expect_equal(dim(selected_coma2), c(1, 4))
  expect_s3_class(selected_coma2$region[[1]], "stars")
})
