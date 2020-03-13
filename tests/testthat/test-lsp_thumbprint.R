context("thumbprint")

result_coma = lsp_thumbprint(landform, type = "coma", threshold = 1)
result_coma500 = lsp_thumbprint(landform, type = "coma",
        threshold = 0.5, window_size = 500)
result_coma500p = lsp_thumbprint(landform_p, type = "coma",
                                 threshold = 0.5, window_size = 500)
result_comawindow = lsp_thumbprint(landform, type = "coma",
                                   threshold = 0.5, window = ecoregions["id"])
# result_comawindowp = lsp_thumbprint(landform_p, type = "coma",
#                                    threshold = 0.5, window = ecoregions["id"])
result_cove500 = lsp_thumbprint(landform, type = "cove",
                                threshold = 0.5, window_size = 500)
result_one_class = lsp_thumbprint(landform, type = "coma",
                                  threshold = 0.9, classes = 10)
result_wecove = lsp_thumbprint(c(landform, random_ndvi),
                               type = "wecove", threshold = 0.9)
result_incove = lsp_thumbprint(c(landcover, landform),
                               type = "incove", threshold = 0.9)
my_fun = function(x){unlist(lapply(x, mean, na.rm = TRUE))}
result_fun = lsp_thumbprint(landform, type = my_fun, threshold = 0.9)

test_that("tests external function", {
  expect_equivalent(result_fun$signature[[1]], 7.333453,
               tolerance = .001)
})

test_that("tests composition", {
  expect_warning({result_composition = lsp_thumbprint(c(landcover, landform),
                                      type = "composition", threshold = 0.9)})
  expect_equal(ncol(result_composition$signature[[1]]), length(unique(c(landcover$landcover2015.tif))))
})

test_that("the output structure is correct", {
  expect_equal(dim(result_coma), c(1, 3))
  expect_equal(dim(result_coma500), c(34, 3))
})

test_that("the output signature class is correct", {
  expect_equal(class(result_coma500$signature[[1]]), "matrix")
  expect_equal(class(result_cove500$signature[[1]]), "matrix")
  expect_equal(nrow(result_coma500$signature[[1]]), 15)
  expect_equal(ncol(result_coma500$signature[[1]]), 15)
  expect_equal(nrow(result_cove500$signature[[1]]), 1)
  expect_equal(dim(result_one_class$signature[[1]]), c(1, 1))
})

test_that("thumprint works corectly for whole area", {
  expect_equal(result_coma$signature[[1]],
               comat::get_coma(landform$landform.tif))
})

test_that("stars results are equal to stars.proxy results", {
  expect_equal(result_coma500$signature[[1]],
               result_coma500p$signature[[1]])
  # expect_equal(result_comawindow$signature[[1]],
  #              result_comawindowp$signature[[1]])
})

test_that("thumprint works corectly for window", {
  expect_equal(nrow(result_comawindow),
               nrow(ecoregions))
  expect_equal(result_comawindow$id,
               ecoregions$id)
})

test_that("wecoma works corectly", {
  expect_equal(result_wecove$na_prop,
               0.681, tolerance = .001)
  expect_equal(ncol(result_wecove$signature[[1]]), 225)
})

test_that("incoma works corectly", {
  expect_equal(result_incove$na_prop,
               0.340, tolerance = .001)
  expect_equal(ncol(result_incove$signature[[1]]), 1156)
})
