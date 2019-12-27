distance2 = function(P, Q, method, p = NULL, test.na = TRUE, unit, est.prob = NULL){
  x = rbind(P, Q)
  message("Metric: '", method, "' using unit: '", unit, "'.")
  suppressMessages(philentropy::distance(x,
                        method = method,
                        p = p,
                        test.na = test.na,
                        unit = unit,
                        est.prob = est.prob))
}

get_unique_values_proxy = function(x, window_size, nr, nc){
  single_guv = function(i, x_path, window_size, nr, nc){
    rasterio = list(nXOff = 1,
                    nYOff = i,
                    nXSize = nr,
                    nYSize = ifelse(i + window_size > nc, nc - i + 1, window_size))
    x_vals = stars::read_stars(x_path, RasterIO = rasterio)
    lapply(x_vals, get_unique_values, TRUE)
  }
  layer_guv = function(x_path, window_size, nr, nc){
    yoffs = seq(1, nc, by = window_size)
    sort(unique(unlist(lapply(yoffs, single_guv, x_path, window_size, nr, nc))))
  }
  lapply(x, layer_guv, window_size, nr, nc)
}

merge_and_update = function(result, window_size, nr){
  update_id = function(multiplier, x, window_size, nr){
    n = ceiling(nr / window_size)
    x[["id"]] = x[["id"]] + (multiplier * n)
    x
  }
  len_x = seq_along(result) - 1
  result = mapply(update_id, len_x, result, window_size, nr, SIMPLIFY = FALSE)
  result = do.call(rbind, result)
  result
}
