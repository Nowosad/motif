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
  type = "wecove",
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

s_incovewindow = lsp_search(
  c(landcover_ext, landform_ext),
  c(landcover, landform),
  type = "incove",
  dist_fun = "jensen-shannon",
  threshold = 0.9,
  window = ecoregions["id"]
)

s_cove_motiel = lsp_search(
  landform_ext,
  landform,
  type = "cove",
  dist_fun = "jensen-shannon",
  threshold = 0.5,
  window = 250
)

s_cove_proxy = lsp_search(
  landform_ext,
  landform_p,
  type = "cove",
  dist_fun = "jensen-shannon",
  threshold = 0.5,
  window = 250
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
                    c(1, 0.0982, 0.4523),
                    tolerance = .001)
  expect_equivalent(unlist(unique(s_cocove)),
                    c(1, 0.0555, 0.4532),
                    tolerance = .001)
  expect_equivalent(unlist(unique(s_wecove)),
                    c(1, 0.0982, 0.451),
                    tolerance = .001)
  expect_equivalent(unlist(unique(s_incove)),
                    c(1, 0.0982, 0.3477),
                    tolerance = .001)
})

test_that("tests motifel search results", {
  expect_equal(sum(is.na(s_cove_motiel$dist)), 2)
  expect_equivalent(min(s_cove_motiel$dist, na.rm = TRUE), 0.006, tolerance = .001)
  expect_equivalent(mean(s_cove_motiel$dist, na.rm = TRUE), 0.589, tolerance = .001)
  expect_equivalent(max(s_cove_motiel$dist, na.rm = TRUE), 1, tolerance = .001)
})

test_that("tests proxy search results", {
  expect_equal(s_cove_proxy, s_cove_motiel)
})

test_that("tests window search results", {
  expect_equal(sort(unique(c(s_cove_window$id))), c(5, 6, 11, 16, 17))
  expect_equivalent(min(s_cove_window$dist, na.rm = TRUE), 0.00029, tolerance = .001)
  expect_equal(mean(c(s_incovewindow$dist), na.rm = TRUE), 0.4455153, tolerance = .001)
})

