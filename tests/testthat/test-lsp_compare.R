library(stars)

lc15 = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
lc01 = read_stars(system.file("raster/landcover2001.tif", package = "motif"))
lf = read_stars(system.file("raster/landform.tif", package = "motif"))
ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))

lc15p = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = TRUE)
lc01p = read_stars(system.file("raster/landcover2001.tif", package = "motif"), proxy = TRUE)

# all area ----------------------------------------------------------------
s1 = lsp_compare(lc01, lc15, type = "cove",
  dist_fun = "jensen-shannon", threshold = 0.9)

# window ------------------------------------------------------------------
s1b = lsp_compare(lc01, lc15, type = "cove",
  dist_fun = "jensen-shannon", window = ecoregions["id"], threshold = 0.9)

# motifel -----------------------------------------------------------------
s1c = lsp_compare(lc01, lc15, type = "cove",
  dist_fun = "jensen-shannon", window_size = 100, threshold = 0.9)

# cocove ------------------------------------------------------------------
s2 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "cocove",
  dist_fun = "jensen-shannon", threshold = 0.9)

# wecove ------------------------------------------------------------------
s3 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "wecove",
  dist_fun = "jensen-shannon", threshold = 0.9)

# incove ------------------------------------------------------------------
s4 = lsp_compare(c(lc01, lf), c(lc15, lf), type = "incove",
  dist_fun = "jensen-shannon", threshold = 0.9)

# proxy motifel -----------------------------------------------------------
s5 = lsp_compare(lc01p, lc15p, type = "cove",
                  dist_fun = "jensen-shannon", window_size = 100, threshold = 0.9)

# all.equal(s1c$dist, s5$dist)

# proxy window ------------------------------------------------------------
s6 = lsp_compare(lc01p, lc15p, type = "cove",
                  dist_fun = "jensen-shannon", window = ecoregions["id"], threshold = 0.9)

# all.equal(s1b$dist, s6$dist)
