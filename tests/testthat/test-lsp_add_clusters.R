context("cluster")

landform_cove = lsp_signature(landform,
                               type = "cove",
                               window = 200,
                               threshold = 0.5,
                               normalization = "pdf")

landform_dist = lsp_to_dist(landform_cove,
                            dist_fun = "jensen-shannon")

landform_hclust = hclust(landform_dist, method = "ward.D2")
# plot(landform_hclust)
clusters = cutree(landform_hclust, k = 6)

# dput(rcartocolor::carto_pal(n=6, "Safe"))
safe_pal = c("#88CCEE", "#CC6677", "#DDCC77",
             "#117733", "#332288", "#888888")

# stars -------------------------------------------------------------------
suppressWarnings({landform_grid_stars = lsp_add_clusters(landform_cove, clusters, output = "stars")})
# plot(landform_grid_stars["clust"], col = safe_pal)

# landform_grid_starsq = lsp_add_quality(landform_grid_stars,
#                                       landform_dist)

# plot(landform_grid_stars["inhomogeneity"])
# plot(landform_grid_stars["isolation"])
# plot(landform_grid_stars["quality"])

# sf ----------------------------------------------------------------------
landform_grid_sf = lsp_add_clusters(landform_cove,
                                    clusters)

# landform_grid_sf$clust = as.factor(landform_grid_sf$clust)
# plot(landform_grid_sf["clust"], pal = safe_pal)
# mapview::mapview(landform_grid_sf["clust"])

landform_grid_sfq_c = lsp_add_quality(landform_grid_sf, landform_dist)
landform_grid_sfq_s = lsp_add_quality(landform_grid_sf, landform_dist, type = "segmentation")

# plot(landform_grid_sf["inhomogeneity"])
# plot(landform_grid_sf["isolation"])
# plot(landform_grid_sf["quality"])

# tests -------------------------------------------------------------------
test_that("tests lsp_to_dist works", {
  expect_s3_class(landform_dist, "dist")
  expect_equal(length(landform_dist) * 2 + 9, nrow(landform_cove)^2)
})

test_that("tests lsp_add_clusters works on stars", {
  expect_s3_class(landform_grid_stars, "stars")
  expect_equal(sort(unique(c(landform_grid_stars$clust))), 1:6)
})

test_that("tests lsp_add_clusters works on sf", {
  expect_s3_class(landform_grid_sf, "sf")
  expect_equal(sort(unique(landform_grid_sf$clust)), 1:6)
})

test_that("tests lsp_add_quality works", {
  # expect_s3_class(landform_grid_starsq, "stars")
  expect_s3_class(landform_grid_sfq_c, "sf")
  # expect_equal(mean(landform_grid_starsq$quality, na.rm = TRUE),
  #              mean(landform_grid_sfq$quality, na.rm = TRUE))
  expect_equal(mean(landform_grid_sfq_c$quality, na.rm = TRUE), 0.987, tolerance = .001)
  expect_equal(mean(landform_grid_sfq_s$quality, na.rm = TRUE), 0.957, tolerance = .001)
})

test_that("region = TRUE is not implemented", {
  expect_error(landform_grid_starsq = lsp_add_quality(landform_grid_stars,
                                         landform_dist,
                                         regions = TRUE))
})

