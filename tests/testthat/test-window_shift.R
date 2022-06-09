context("window")
# library(sf)
landform_cove = lsp_signature(landform,
                              type = "cove",
                              window = c(200, 100),
                              threshold = 1,
                              normalization = "pdf")

landform_lsp_sf = lsp_add_sf(landform, window = c(200, 100))
plot(landform_lsp_sf[1])

landform_lsp_cove = lsp_add_sf(landform_cove)
plot(landform_lsp_cove[1])


s_cove = lsp_search(landform_ext,
                    landform,
                    type = "cove",
                    dist_fun = "jensen-shannon",
                    threshold = 0.9,
                    window = c(200, 100))

landform_dist = lsp_to_dist(landform_cove,
                            dist_fun = "jensen-shannon")
