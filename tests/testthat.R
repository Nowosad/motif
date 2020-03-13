library(testthat)
library(stars)
library(motif)

landform = read_stars(system.file("raster/landform.tif", package = "motif"))
landform_p = read_stars(system.file("raster/landform.tif", package = "motif"), proxy = TRUE)
ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))

test_check("motif")
