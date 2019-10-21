#' Create a grid of square-shaped windows
#'
#' @param x A RasterLayer/RasterStack/RasterBrick
#' @param size Expressed in the numbers of cells, is a length of the side of a square-shaped block of cells. It defines the extent of a local pattern. If `size=NULL` calculations are perfomed for the whole area
#' @param shift Defines the shift between adjacent squares of cells along with the N-S and W-E directions. It describes the density (resolution) of the output grid. The resolution of the output map will be reduced to the original resolution multiplied by the shift. If shift=size the input map will be divided into a grid of non-overlapping square windows. Each square window defines the extent of a local pattern. If shift < size - results in the grid of overlapping square windows.
#'
#' @return An sf polygon object
#'
#' @aliases lop_centroids
#' @rdname lop_centroids
#'
#' @examples
#' library(comat)
#' library(raster)
#' landcover = raster(system.file("raster/landcover.tif", package = "lopata"))
#' # plot(landcover)
#'
#' landcover_grid = lop_centroids(landcover, size = 100)
#' landcover_grid
#'
#' @export
lop_centroids = function(x, size, shift = NULL) UseMethod("lop_centroids")

#' @name lop_centroids
#' @export
lop_centroids.RasterLayer = function(x, size, shift = NULL){

  if (is.null(shift)){
    shift = size
  }

  bb = sf::st_bbox(raster::extent(x))
  offset = bb[c("xmin", "ymax")]

  cellsize = c((size * raster::res(x)[1]), (size * raster::res(x)[2]))
  cellshift = c(((shift) * raster::res(x)[[1]]),
                ((shift) * raster::res(x)[[2]]))

  nx = ceiling(abs((bb[3] - offset[1]) / cellshift[1]))
  ny = ceiling(abs((bb[2] - offset[2]) / cellshift[2]))

  xc = offset[1] + (seq_len(nx)) * cellshift[1]
  yc = offset[2] - (seq_len(ny)) * cellshift[2]

  xy_coords = expand.grid(xc, yc)

  my_centr = sfheaders::sfc_point(xy_coords)

  my_centr = sf::st_sf(geom = my_centr)
  sf::st_crs(my_centr) = NA
  sf::st_crs(my_centr) = sf::st_crs(x)

  if (raster::nlayers(x) == 1){
    df_ids = get_motifels_ids(raster::nrow(x), raster::ncol(x), size, shift)
  } else {
    df_ids = get_motifels_ids(raster::nrow(x[[1]]), raster::ncol(x[[1]]), size, shift)
  }

  my_centr = cbind(df_ids, my_centr)
  colnames(my_centr) = c("row", "col", "geom")


  # df_ids = raceland:::create_motifels_ids(raster::as.matrix(x[[1]]), size, shift)
  #
  # my_centr = cbind(df_ids, my_centr)
  # colnames(my_centr) = c("row", "col", "geom")

  return(my_centr)
}
