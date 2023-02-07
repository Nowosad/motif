context("thumbprint")

result_coma = lsp_signature(landform, type = "coma", threshold = 1)
result_compositionwindow = lsp_signature(landform, type = "composition",
                                         threshold = 1, window = ecoregions["id"])

result_coma500 = lsp_signature(landform, type = "coma",
                               threshold = 1, window = 500)
result_comap = lsp_signature(landform_p, type = "coma",
                             threshold = 1)
result_coma500p = lsp_signature(landform_p, type = "coma",
                                threshold = 0.5, window = 500)
result_comawindow = lsp_signature(landform, type = "coma",
                                  threshold = 0.5, window = ecoregions["id"])
suppressWarnings({result_comawindowp = lsp_signature(landform_p, type = "coma",
                                   threshold = 0.5, window = ecoregions[1, "id"])})
# result_comawindowp = lsp_signature(landform_p, type = "coma",
#                                    threshold = 0.5, window = ecoregions["id"])
result_cove500 = lsp_signature(landform, type = "cove",
                               threshold = 0.5, window = 500)
result_one_class = lsp_signature(landform, type = "coma",
                                 threshold = 0.9, classes = 10)
result_cocove1000 = lsp_signature(c(landform, landcover),
                                  type = "cocove", threshold = 0.9,
                                  window = 1000)
result_cocovewindow = lsp_signature(c(landform, landcover),
                                    type = "cocove", threshold = 0.1,
                                    window = ecoregions["id"])
result_wecove = lsp_signature(c(landform, random_ndvi),
                              type = "wecove", threshold = 0.9)
result_wecove1000 = lsp_signature(c(landform, random_ndvi),
                                  type = "wecove", threshold = 0.1,
                                  window = 1000)
result_wecovewindow = lsp_signature(c(landform, random_ndvi),
                                    type = "wecove", threshold = 0.1,
                                    window = ecoregions["id"])

result_incove = lsp_signature(c(landcover, landform),
                              type = "incove", threshold = 0.9)
result_incove_p = lsp_signature(c(landcover_p, landform_p),
                                type = "incoma", threshold = 0.9,
                                window = 500)

result_fun = lsp_signature(landform, type = my_fun, threshold = 0.9)
result_funwindow = lsp_signature(landform, type = my_fun, threshold = 0.9, window = ecoregions["id"])
result_fun500 = lsp_signature(landform, type = my_fun, threshold = 0.5,
                              window = 500)

test_that("tests external function", {
  expect_equivalent(result_fun$signature[[1]], 1,
                    tolerance = .001)
  expect_equivalent(result_fun500$signature[[1]], 1,
                    tolerance = .001)
})

test_that("tests composition", {
  expect_warning({result_composition = lsp_signature(c(landcover, landform),
                                                     type = "composition", threshold = 0.9)})
  expect_equal(ncol(result_composition$signature[[1]]),
               length(sort(unique(c(landcover$landcover2015s.tif)))))
})

test_that("the output structure is correct", {
  expect_equal(dim(result_coma), c(1, 3))
  expect_equal(dim(result_coma500), c(4, 3))
})

test_that("the output signature class is correct", {
  expect_is(result_coma500$signature[[1]], "matrix")
  expect_is(result_cove500$signature[[1]], "matrix")
  expect_equal(nrow(result_coma500$signature[[1]]), 12)
  expect_equal(ncol(result_coma500$signature[[1]]), 12)
  expect_equal(nrow(result_cove500$signature[[1]]), 1)
  expect_equal(dim(result_one_class$signature[[1]]), c(1, 1))
})

test_that("thumprint works corectly for whole area", {
  expect_equal(result_coma$signature[[1]],
               comat::get_coma(landform$landforms.tif))
})

test_that("stars results are equal to stars.proxy results", {
  expect_equal(result_comap,
               result_coma)
  expect_equal(result_coma500$signature[[1]],
               result_coma500p$signature[[1]])
  expect_equal(result_comawindow$signature[[1]],
               result_comawindowp$signature[[1]])
})

test_that("thumprint works corectly for window", {
  expect_equal(nrow(result_comawindow),
               nrow(ecoregions))
  expect_equal(result_comawindow$id,
               ecoregions$id)
  expect_equal(length(result_funwindow$signature[[1]]), 1)
  expect_equal(length(result_compositionwindow$signature[[1]]), 12)
})

test_that("wecoma works corectly", {
  expect_equal(result_wecove$na_prop,
               0.0982, tolerance = .001)
  expect_equal(ncol(result_wecove$signature[[1]]), 78)
})

test_that("cocove works corectly", {
  expect_equal(ncol(result_cocove1000$signature[[1]]), 28)
})

test_that("incoma works corectly", {
  expect_equal(result_incove$na_prop,
               0.0982, tolerance = .001)
  expect_equal(ncol(result_incove$signature[[1]]), 190)
})

test_that("na_prop works correctly", {
  expect_equal(result_cocovewindow$na_prop, result_wecovewindow$na_prop)
})

