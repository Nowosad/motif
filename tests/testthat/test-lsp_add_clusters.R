context("cluster")

landform_cove = lsp_thumbprint(landform,
                               type = "cove",
                               window_size = 200,
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
landform_grid_stars = lsp_add_clusters(landform_cove, clusters)
# plot(landform_grid_stars["clust"], col = safe_pal)

# sf ----------------------------------------------------------------------
landform_grid_sf = lsp_add_clusters(landform_cove,
                                    clusters, output = "sf")

# landform_grid_sf$clust = as.factor(landform_grid_sf$clust)
# plot(landform_grid_sf["clust"], pal = safe_pal)
# mapview::mapview(landform_grid_sf["clust"])

test_that("tests lsp_to_dist works", {
  expect_s3_class(landform_dist, "dist")
  expect_equal(length(landform_dist) * 2 + 223, nrow(landform_cove)^2)
})

test_that("tests lsp_add_clusters works on stars", {
  expect_s3_class(landform_grid_stars, "stars")
  expect_equal(sort(unique(c(landform_grid_stars$clust))), 1:6)
})

test_that("tests lsp_add_clusters works on sf", {
  expect_s3_class(landform_grid_sf, "sf")
  expect_equal(sort(unique(landform_grid_sf$clust)), 1:6)
})
