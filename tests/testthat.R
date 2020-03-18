library(testthat)
library(stars)
library(sf)
library(motif)

# prepare regular stars ---------------------------------------------------
landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
landform = read_stars(system.file("raster/landform.tif", package = "motif"))
landcoverold = read_stars(system.file("raster/landcover2001.tif", package = "motif"))

# prepare stars proxy -----------------------------------------------------
landform_p = read_stars(system.file("raster/landform.tif", package = "motif"), proxy = TRUE)
landcover_p = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = TRUE)
landcoverold_p = read_stars(system.file("raster/landcover2001.tif", package = "motif"), proxy = TRUE)

# prepare poly ------------------------------------------------------------
ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
# st_crs(ecoregions) = st_crs(landcover)

# prepare cont data -------------------------------------------------------
set.seed(222)
random_ndvi = landcover
random_ndvi$ndvi = sample(x = 1:10, size = length(random_ndvi[[1]]), replace = TRUE)
random_ndvi$landcover2015.tif = NULL

# prepare sample landscapes -----------------------------------------------
ext = st_bbox(c(xmin = -249797.344531127, xmax = -211162.693944285,
                ymin = -597280.143035389, ymax = -558645.492448547),
                crs = st_crs(landcover))

landcover_ext = landcover[ext]
landform_ext = landform[ext]
random_ndvi_ext = random_ndvi[ext]

# prepare my fun ----------------------------------------------------------
my_fun = function(x) sum(!is.na(c(x[[1]])))

test_check("motif")
