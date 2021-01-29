library(testthat)
library(stars)
library(sf)
library(motif)

# prepare regular stars ---------------------------------------------------
# landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = FALSE)
# landform = read_stars(system.file("raster/landform.tif", package = "motif"), proxy = FALSE)
# landcoverold = read_stars(system.file("raster/landcover2001.tif", package = "motif"), proxy = FALSE)

# prepare stars proxy -----------------------------------------------------
# landform_p = read_stars(system.file("raster/landform.tif", package = "motif"), proxy = TRUE)
# landcover_p = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = TRUE)
# landcoverold_p = read_stars(system.file("raster/landcover2001.tif", package = "motif"), proxy = TRUE)

# prep test extent --------------------------------------------------------
# t_ext = st_bbox(c(xmin = -400000, xmax = -200000,
#                 ymin = -600000, ymax = -400000),
#               crs = st_crs(landcover))

# prepare regular stars ---------------------------------------------------
# landcover = landcover[t_ext]
# landform = landform[t_ext]
# landcoverold = landcoverold[t_ext]
landcover = read_stars(system.file("raster/landcover2015s.tif", package = "motif"), proxy = FALSE)
landform = read_stars(system.file("raster/landforms.tif", package = "motif"), proxy = FALSE)
landcoverold = read_stars(system.file("raster/landcover2001s.tif", package = "motif"), proxy = FALSE)

# prepare stars proxy -----------------------------------------------------
# landform_p = st_crop(landform_p, t_ext)
# landcover_p = st_crop(landcover_p, t_ext)
# landcoverold_p = st_crop(landcoverold_p, t_ext)
landcover_p = read_stars(system.file("raster/landcover2015s.tif", package = "motif"), proxy = TRUE)
landform_p = read_stars(system.file("raster/landforms.tif", package = "motif"), proxy = TRUE)
landcoverold_p = read_stars(system.file("raster/landcover2001s.tif", package = "motif"), proxy = TRUE)

# prepare poly ------------------------------------------------------------
# ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
# # st_crs(ecoregions) = st_crs(landform_p)
# ecoregions = st_transform(ecoregions, st_crs(landform_p))
# ecoregions = st_crop(ecoregions, t_ext)
# ecoregions = st_cast(ecoregions, "MULTIPOLYGON")
ecoregions = read_sf(system.file("vector/ecoregionss.gpkg", package = "motif"))
ecoregions = st_transform(ecoregions, st_crs(landform))

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
