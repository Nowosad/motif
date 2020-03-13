get_window_single_proxy = function(window_id, x, window, ...){
  # print(window_id)
  result = lsp_thumbprint(stars::st_as_stars(x[window[window_id, ]]), ...)
  if (nrow(result) == 1){
    result$id = window[[window_id, 1]]
    result$na_prop = NA
  }
  return(result)
}

get_motifels_single_proxy = function(i, x_path, type, directions, window_size, window_shift, f, threshold, classes, wecoma_fun, wecoma_na_action, nr, nc){
  rasterio = list(nXOff = 1, nYOff = i, nXSize = nr,
                  nYSize = ifelse(i + window_size > nc,
                                  nc - i + 1,
                                  window_size))
  x = stars::read_stars(unlist(x_path), RasterIO = rasterio)
  x = lapply(x, function(x) `mode<-`(x, "integer"))
  x = get_motifels(x,
                   type = type,
                   directions = directions,
                   size = window_size,
                   shift = window_shift,
                   f = f,
                   threshold = threshold,
                   classes = classes,
                   fun = wecoma_fun,
                   na_action = wecoma_na_action)
  x = tibble::as_tibble(x)
  x
}
