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

#
# # all area ----------------------------------------------------------------
# s1 = lsp_search(lc_ext, lc, type = "cove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # cocove ------------------------------------------------------------------
# s2 = lsp_search(c(lc_ext, lf_ext), c(lc, lf), type = "cocove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # wecove ------------------------------------------------------------------
# s3 = lsp_search(c(lc_ext, rn_ext), c(lc, rn), type = "wecove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # incove ------------------------------------------------------------------
# s4 = lsp_search(c(lc_ext, lf_ext), c(lc, lf), type = "incove",
#   dist_fun = "jensen-shannon", threshold = 0.9)
#
# # motifel -----------------------------------------------------------------
# s5 = lsp_search(lf_ext, lf, type = "cove",
#   dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250)
#
# # window ------------------------------------------------------------------
# s6 = lsp_search(lc_ext, lc, type = "cove",
#   dist_fun = "jensen-shannon", threshold = 1, window = ecoregions["id"])
#
# # motifel proxy -----------------------------------------------------------
# s7 = lsp_search(lf_ext, lfp, type = "cove",
#                 dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250)
#
# # window proxy ------------------------------------------------------------
# s8 = lsp_search(lc_ext, lcp, type = "cove",
#                 dist_fun = "jensen-shannon", threshold = 1, window = ecoregions["id"])
#
# # all.equal(s6, s8)
# # bench::mark(lsp_search(lf_ext, lf, type = "cove",
# #                        dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250),
# #             lsp_search(lf_ext, lfp, type = "cove",
# #                        dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250))
