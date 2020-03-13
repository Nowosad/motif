library(testthat)
library(stars)
library(motif)

landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
landform = read_stars(system.file("raster/landform.tif", package = "motif"))
landform_p = read_stars(system.file("raster/landform.tif", package = "motif"), proxy = TRUE)
ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
set.seed(222)
random_ndvi = landcover
random_ndvi$ndvi = sample(x = 1:10, size = length(random_ndvi[[1]]), replace = TRUE)
random_ndvi$landcover2015.tif = NULL

test_check("motif")
