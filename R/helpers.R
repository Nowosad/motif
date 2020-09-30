distance2 = function(P, Q, method, p = NULL, test.na = TRUE, unit, est.prob = NULL){
  x = rbind(P, Q)
  suppressMessages(philentropy::distance(x,
                        method = method,
                        p = p,
                        test.na = test.na,
                        unit = unit,
                        est.prob = est.prob))
}

get_unique_values_proxy2 = function(x, window_size){
  guvp_one_layer = function(x_path, window_size, dimensions){
    nc = ncol(dimensions)
    nr = nrow(dimensions)
    start_x = dimensions[[1]][[1]]
    start_y = dimensions[[2]][[1]]

    if (window_size == 0) window_size = nc
    shift = seq(1, nc, by = window_size)
    all_vals = vector(mode = "list", length = length(shift))
    for (i in seq_along(shift)){
      ny_size = ifelse(shift[i] + window_size > nc,
                       nc - shift[i] + 1,
                       window_size)
      rasterio = list(nXOff = start_x,
                      nYOff = (start_y - 1) + shift[i],
                      nXSize = nr,
                      nYSize = ny_size)
      x_vals = stars::read_stars(x_path, RasterIO = rasterio, proxy = FALSE)
      all_vals[i] = lapply(x_vals, get_unique_values, TRUE)
    }
    all_unique_vals = sort(unique(unlist(all_vals)))
    return(all_unique_vals)
  }
  dimensions = stars::st_dimensions(x)
  lapply(x, guvp_one_layer, window_size, dimensions)
}

# get_unique_values_proxy = function(x, window_size, nr, nc){
#   single_guv = function(i, x_path, window_size, nr, nc){
#     ny_size = ifelse(i + window_size > nc, nc - i + 1, window_size)
#     xstart = st_dimensions(x_path)[[1]][[1]]
#     ystart = st_dimensions(x_path)[[2]][[1]]
#     rasterio = list(nXOff = xstart,
#                     nYOff = ystart + i - 1,
#                     nXSize = xstart + nr - 1,
#                     nYSize = ystart + ny_size - 1)
#     x_vals = stars::read_stars(x_path, RasterIO = rasterio, proxy = FALSE)
#     lapply(x_vals, get_unique_values, TRUE)
#   }
#   layer_guv = function(x_path, window_size, nr, nc){
#     if (window_size == 0) window_size = nc
#     yoffs = seq(1, nc, by = window_size)
#     sort(unique(unlist(lapply(yoffs, single_guv, x_path, window_size, nr, nc))))
#   }
#   lapply(x, layer_guv, window_size, nr, nc)
# }


normalize_signature = function(x, normalization){
  if (normalization == "pdf"){
    return(x / sum(x))
  }
}
