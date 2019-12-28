get_motifels_fun_single_proxy = function(i, x_path, window_size, window_shift, f, threshold, classes, nr, nc){
  rasterio = list(nXOff = 1,
                  nYOff = i,
                  nXSize = nr,
                  nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
  x = stars::read_stars(x_path, RasterIO = rasterio)
  x = lapply(x, function(x) `mode<-`(x, "integer"))
  x = get_motifels_fun(x,
                        size = window_size,
                        shift = window_shift,
                        f = f,
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
  x = stars::read_stars(x_path, RasterIO = rasterio)
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
  x = stars::read_stars(x_path, RasterIO = rasterio)
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
  x = stars::read_stars(x_path, RasterIO = rasterio)
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
  x = stars::read_stars(x_path, RasterIO = rasterio)
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

get_window_single_proxy = function(window_id, x, window, ...){
  # print(window_id)
  result = lsp_thumbprint(stars::st_as_stars(x[window[window_id, ]]), ...)
  if (nrow(result) == 1){
    result$id = window[[window_id, 1]]
    result$na_prop = NA
  }
  return(result)
}


# get_motifels_coma_single_proxy = function(i, x_path, directions, m, threshold, classes, nr, nc){
#   rasterio = list(nXOff = 1,
#                   nYOff = i,
#                   nXSize = nr,
#                   nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
#   x = stars::read_stars(x_path, RasterIO = rasterio)
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
