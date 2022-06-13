context("window")
# library(sf)
# devtools::load_all()


# signature ---------------------------------------------------------------
landform_cove = lsp_signature(landform,
                              type = "cove",
                              window = c(200, 100),
                              threshold = 1,
                              normalization = "pdf")

# add spatial -------------------------------------------------------------
landform_lsp_sf = lsp_add_sf(landform, window = c(200, 100))
plot(landform_lsp_sf[1])

landform_lsp_cove = lsp_add_sf(landform_cove)
plot(landform_lsp_cove[1])

landform_lsp_cove_stars = lsp_add_stars(landform_cove)
plot(landform_lsp_cove_stars[1])

landform_lsp_stars = lsp_add_stars(landform, window = c(200, 100))
plot(landform_lsp_stars[1])

library(terra)
landform_lsp_cove_terra = lsp_add_terra(landform_cove)
plot(landform_lsp_cove_terra)

landform_lsp_terra = lsp_add_terra(landform, window = c(200, 100))
plot(landform_lsp_terra)

# search ------------------------------------------------------------------
s_cove = lsp_search(landform_ext,
                    landform,
                    type = "cove",
                    dist_fun = "jensen-shannon",
                    threshold = 0.9,
                    window = c(200, 100))

plot(s_cove)


# clusters ----------------------------------------------------------------
landform_cove = lsp_signature(landform, type = "cove", window = c(200, 100),
                              threshold = 0.5, normalization = "pdf")

landform_dist = lsp_to_dist(landform_cove, dist_fun = "jensen-shannon")

landform_hclust = hclust(landform_dist, method = "ward.D2")
clusters = cutree(landform_hclust, k = 6)

safe_pal = c("#88CCEE", "#CC6677", "#DDCC77",
             "#117733", "#332288", "#888888")

# stars -------------------------------------------------------------------
suppressWarnings({landform_grid_stars = lsp_add_clusters(landform_cove, clusters, output = "stars")})
plot(landform_grid_stars["clust"], col = safe_pal)

# sf ----------------------------------------------------------------------
landform_grid_sf = lsp_add_clusters(landform_cove, clusters)

landform_grid_sfq_c = lsp_add_quality(landform_grid_sf, landform_dist)
landform_grid_sfq_s = lsp_add_quality(landform_grid_sf, landform_dist, type = "segmentation")

landform_grid_sf_sel = landform_grid_sf %>%
  dplyr::filter(na_prop < 0.01) %>%
  dplyr::group_by(clust) %>%
  dplyr::slice_sample(n = 4, replace = TRUE)

landform_grid_sf_sel = lsp_add_examples(x = landform_grid_sf_sel, y = landform)

landform_clust_m = lsp_mosaic(landform_grid_sf_sel)

