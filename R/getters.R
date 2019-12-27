# get_comaa = function(x,
#                     directions,
#                     window_size,
#                     window_shift,
#                     threshold,
#                     classes){
#
#
#   get_coma_single = function(i, x_path, window_size, nr, nc, classes, directions){
#     rasterio = list(nXOff = 1,
#                     nYOff = i,
#                     nXSize = nr,
#                     nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
#     # print(x_path)
#     # print(i)
#     x = read_stars(x_path, RasterIO = rasterio)
#     x = lapply(x, function(x) `mode<-`(x, "integer"))
#     x = get_motifels_coma(x[[1]],
#                           directions = directions,
#                           size = window_size,
#                           shift = window_shift,
#                           threshold = threshold,
#                           classes = classes[1])
#     x = tibble::as_tibble(x)
#     x
#   }
#
#   nc = ncol(x)
#   nr = nrow(x)
#
#   classes = get_unique_values_proxy(x, window_size, nr, nc)
#
#   yoffs = seq(1, nc, by = window_size)
#   result = lapply(yoffs, get_coma_single, x[[1]], window_size, nr, nc, classes, directions)
#
#   result = merge_and_update(result, window_size, nr)
#
# }
#
# library(stars)
# x = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = TRUE)
#
# a = get_comaa(x, directions = as.matrix(neighbourhood <- 4), window_size = 100,
#              window_shift = 100, threshold = 0.5)
#
# x2 = read_stars(system.file("raster/landcover2015.tif", package = "motif"), proxy = FALSE)
# b = lsp_thumbprint(x2, type = "coma", neighbourhood = 4, window_size = 100,
#                    window_shift = 100, threshold = 0.5)
# b
#
#
# bench::mark(get_comaa(x, directions = as.matrix(neighbourhood <- 4), window_size = 100,
#                       window_shift = 100, threshold = 0.5),
#             lsp_thumbprint(x2, type = "coma", neighbourhood = 4, window_size = 100,
#                            window_shift = 100, threshold = 0.5),
#             check = FALSE)
#
# profvis::profvis(get_comaa(x, directions = as.matrix(neighbourhood <- 4), window_size = 100,
#                            window_shift = 100, threshold = 0.5)
#                  )
#
#
# x = read_stars("../../sil/global_changes_in_comp_and_conf/data/LC_1994.tif", proxy = TRUE)
# dd = bench::mark({ee = get_comaa(x, directions = as.matrix(neighbourhood <- 4), window_size = 1000,
#               window_shift = 1000, threshold = 0.5)})
# ee


get_motifels_fun_single_proxy = function(i, x_path, window_size, window_shift, f, threshold, classes, nr, nc){
  rasterio = list(nXOff = 1,
                  nYOff = i,
                  nXSize = nr,
                  nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
  x = read_stars(x_path, RasterIO = rasterio)
  x = lapply(x, function(x) `mode<-`(x, "integer"))
  x = get_motifels_fun(x,
                        size = window_size,
                        shift = window_shift,
                        f = type,
                        threshold = threshold,
                        classes = classes)
  x = tibble::as_tibble(x)
  x
}

get_motifels_coma_single_proxy = function(i, x_path, directions, window_size, window_shift, threshold, classes, nr, nc){
  rasterio = list(nXOff = 1,
                  nYOff = i,
                  nXSize = nr,
                  nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
  x = read_stars(x_path, RasterIO = rasterio)
  x = lapply(x, function(x) `mode<-`(x, "integer"))
  x = get_motifels_coma(x[[1]],
                        directions = directions,
                        size = window_size,
                        shift = window_shift,
                        threshold = threshold,
                        classes = classes[1])
  x = tibble::as_tibble(x)
  x
}


get_motifels_cocoma_single_proxy = function(i, x_path, directions, window_size, window_shift, threshold, classes, nr, nc){
  rasterio = list(nXOff = 1,
                  nYOff = i,
                  nXSize = nr,
                  nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
  x = read_stars(x_path, RasterIO = rasterio)
  x = lapply(x, function(x) `mode<-`(x, "integer"))
  x = get_motifels_cocoma(x[[1]],
                        x[[2]],
                        directions = directions,
                        size = window_size,
                        shift = window_shift,
                        threshold = threshold,
                        classes = classes)
  x = tibble::as_tibble(x)
  x
}


get_motifels_wecoma_single_proxy = function(i, x_path, directions, window_size, window_shift, threshold, classes, nr, nc){
  rasterio = list(nXOff = 1,
                  nYOff = i,
                  nXSize = nr,
                  nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
  x = read_stars(x_path, RasterIO = rasterio)
  x = lapply(x, function(x) `mode<-`(x, "integer"))
  x = get_motifels_wecoma(x[[1]],
                        x[[2]],
                        directions = directions,
                        size = window_size,
                        shift = window_shift,
                        threshold = threshold,
                        classes = classes)
  x = tibble::as_tibble(x)
  x
}

get_motifels_incoma_single_proxy = function(i, x_path, directions, window_size, window_shift, threshold, classes, nr, nc){
  rasterio = list(nXOff = 1,
                  nYOff = i,
                  nXSize = nr,
                  nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
  x = read_stars(x_path, RasterIO = rasterio)
  x = lapply(x, function(x) `mode<-`(x, "integer"))
  x = get_motifels_incoma(x,
                        directions = directions,
                        size = window_size,
                        shift = window_shift,
                        threshold = threshold,
                        classes = classes)
  x = tibble::as_tibble(x)
  x
}


# get_motifels_coma_single_proxy = function(i, x_path, directions, m, threshold, classes, nr, nc){
#   rasterio = list(nXOff = 1,
#                   nYOff = i,
#                   nXSize = nr,
#                   nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
#   x = read_stars(x_path, RasterIO = rasterio)
#   x = lapply(x, function(x) `mode<-`(x, "integer"))
#   x = get_motifels_coma(x[[1]],
#                         directions = directions,
#                         size = window_size,
#                         shift = window_shift,
#                         threshold = threshold,
#                         classes = classes[1])
#   x = tibble::as_tibble(x)
#   x
# }
