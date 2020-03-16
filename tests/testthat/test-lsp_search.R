context("search")

s_cove = lsp_search(
  landform_ext,
  landform,
  type = "cove",
  dist_fun = "jensen-shannon",
  threshold = 0.9
)

s_cocove = lsp_search(
  c(landcover_ext, landform_ext),
  c(landcover, landform),
  type = "cocove",
  dist_fun = "jensen-shannon",
  threshold = 0.9
)

s_wecove = lsp_search(
  c(landform_ext, random_ndvi_ext),
  c(landform, random_ndvi),
  type = "cocove",
  dist_fun = "jensen-shannon",
  threshold = 0.9
)

s_incove = lsp_search(
  c(landcover_ext, landform_ext),
  c(landcover, landform),
  type = "incove",
  dist_fun = "jensen-shannon",
  threshold = 0.9
)

s_cove_motiel = lsp_search(
  landform_ext,
  landform,
  type = "cove",
  dist_fun = "jensen-shannon",
  threshold = 0.5,
  window_size = 250
)

s_cove_proxy = lsp_search(
  landform_ext,
  landform_p,
  type = "cove",
  dist_fun = "jensen-shannon",
  threshold = 0.5,
  window_size = 250
)

s_cove_window = lsp_search(
  landform_ext,
  landform,
  type = "cove",
  dist_fun = "jensen-shannon",
  threshold = 1,
  window = ecoregions["id"]
)

test_that("tests simple search results", {
  expect_equivalent(unlist(unique(s_cove)),
                    c(1, 0.6806109, 0.4454181),
                    tolerance = .001)
  expect_equivalent(unlist(unique(s_cocove)),
                    c(1, 0, 0.8314),
                    tolerance = .001)
  expect_equivalent(unlist(unique(s_wecove)),
                    c(1, 0.6806, 0.4414),
                    tolerance = .001)
  expect_equivalent(unlist(unique(s_incove)),
                    c(1, 0.3403, 0.7562),
                    tolerance = .001)
})

test_that("tests motifel search results", {
  expect_equal(sum(is.na(s_cove_motiel$dist)), 341)
  expect_equivalent(min(s_cove_motiel$dist, na.rm = TRUE), 0.0001, tolerance = .001)
  expect_equivalent(mean(s_cove_motiel$dist, na.rm = TRUE), 0.5727, tolerance = .001)
  expect_equivalent(max(s_cove_motiel$dist, na.rm = TRUE), 1, tolerance = .001)
})

test_that("tests proxy search results", {
  expect_equal(s_cove_proxy, s_cove_motiel)
})

test_that("tests window search results", {
  expect_equal(sort(unique(c(s_cove_window$id))), 1:22)
  expect_equivalent(min(s_cove_window$dist, na.rm = TRUE), 0.00029, tolerance = .001)
})
