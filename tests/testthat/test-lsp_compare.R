context("compare")

c_cove = lsp_compare(
  landcover,
  landcoverold,
  type = "cove",
  dist_fun = "jensen-shannon",
  threshold = 0.9
)

c_cove_proxy = lsp_compare(
  landcover_p,
  landcoverold_p,
  type = "cove",
  dist_fun = "jensen-shannon",
  threshold = 0.9
)

c_cocove = lsp_compare(
  c(landcover, landform),
  c(landcoverold, landform),
  type = "cocove",
  dist_fun = "jensen-shannon",
  threshold = 0.9
)

c_wecove = lsp_compare(
  c(landcover, random_ndvi),
  c(landcoverold, random_ndvi),
  type = "wecove",
  dist_fun = "jensen-shannon",
  threshold = 0.9
)

c_composition = lsp_compare(
  landcover,
  landcoverold,
  type = "composition",
  dist_fun = "jensen-shannon",
  threshold = 0.9,
  window_size = 500
)

test_that("tests simple compare results", {
  expect_equal(length(c_cove), 4)
  expect_equal(unique(c(c_cove$dist)),
               0.0002, tolerance = .001)
})

test_that("tests proxy compare results", {
  expect_equal(c_cove, c_cove_proxy)
})

test_that("tests compare composition results", {
  expect_equal(mean(c_composition$dist, na.rm = TRUE), 0.00045, tolerance = .001)
})

test_that("tests compare cocove results", {
  expect_equal(c_cocove$dist[[1]], 0.00057, tolerance = .001)
})

test_that("tests compare wecove results", {
  expect_equal(c_wecove$dist[[1]], 0.0002, tolerance = .001)
})

# # all area ----------------------------------------------------------------
# s1 = lsp_compare(lc01, lc15, type = "cove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # window ------------------------------------------------------------------
# s1b = lsp_compare(lc01, lc15, type = "cove",
#   dist_fun = "jensen-shannon", window = ecoregions["id"], threshold = 0.9)
#
# # motifel -----------------------------------------------------------------
# s1c = lsp_compare(lc01, lc15, type = "cove",
#   dist_fun = "jensen-shannon", window_size = 100, threshold = 0.9)
#
# # cocove ------------------------------------------------------------------
# s2 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "cocove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # wecove ------------------------------------------------------------------
# s3 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "wecove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # incove ------------------------------------------------------------------
# s4 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "incove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # proxy motifel -----------------------------------------------------------
# s5 = lsp_compare(lc01p, lc15p, type = "cove",
#                   dist_fun = "jensen-shannon", window_size = 100, threshold = 0.9)
#
# # all.equal(s1c$dist, s5$dist)
#
# # proxy window ------------------------------------------------------------
# s6 = lsp_compare(lc01p, lc15p, type = "cove",
#                   dist_fun = "jensen-shannon", window = ecoregions["id"], threshold = 0.9)
#
# # all.equal(s1b$dist, s6$dist)
