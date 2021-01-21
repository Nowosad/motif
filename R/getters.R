prepare_type = function(type){
  if (type == "cove"){
    type = "coma"
  } else if (type == "cocove"){
    type = "cocoma"
  } else if (type == "wecove"){
    type = "wecoma"
  } else if (type == "incove"){
    type = "incoma"
  }
  return(type)
}

convert_signatures = function(x, type, ordered, repeated, normalization){
  if (!is.function(type)){
    if (type == "cove"){
      x$signature = lapply(x$signature,
                           comat::get_cove,
                           ordered = ordered,
                           normalization = normalization)
    } else if (type == "cocove"){
      x$signature = lapply(x$signature,
                           comat::get_cocove,
                           ordered = ordered,
                           normalization = normalization)
    } else if (type == "wecove"){
      x$signature = lapply(x$signature,
                           comat::get_wecove,
                           ordered = ordered,
                           normalization = normalization)
    } else if (type == "incove"){
      x$signature = lapply(x$signature,
                           comat::get_incove,
                           ordered = ordered,
                           repeated = repeated,
                           normalization = normalization)
    } else if ((type == "composition" || type == "fun") && normalization != "none"){
      x$signature = lapply(x$signature,
                           normalize_signature,
                           normalization = normalization)
    }
  }
  return(x)
}


get_motifels_single_proxy = function(i, x_path, type, directions, window_size,
                                     window_shift, f, threshold, classes,
                                     wecoma_fun, wecoma_na_action, dimensions){

  nc = ncol(dimensions)
  nr = nrow(dimensions)
  start_x = dimensions[[1]][[1]]
  start_y = dimensions[[2]][[1]]

  ny_size = ifelse((i + window_size > nc || i + window_size == 1),
                   nc - i + 1,
                   window_size)
  rasterio = list(nXOff = start_x,
                  nYOff = (start_y - 1) + i,
                  nXSize = unname(nr),
                  nYSize = ny_size)

  # print(rasterio)

  x = stars::read_stars(unlist(x_path), RasterIO = rasterio, proxy = FALSE)
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

merge_and_update = function(result, window_size, nr){
  update_id = function(multiplier, x, window_size, nr){
    n = ifelse(window_size != 0, ceiling(nr / window_size), 1)
    x[["id"]] = x[["id"]] + (multiplier * n)
    x
  }
  len_x = seq_along(result) - 1
  result = mapply(update_id, len_x, result, window_size, nr, SIMPLIFY = FALSE)
  result = do.call(rbind, result)
  result
}

get_motifels_all = function(x, type, directions, window_size, window_shift,
                            f, threshold, classes,
                            ordered, repeated, normalization,
                            wecoma_fun, wecoma_na_action, dimensions){
  type2 = prepare_type(type)
  if (!(inherits(x, "stars_proxy"))){
    x = get_motifels(x,
                     type = type2,
                     directions = directions,
                     size = window_size,
                     shift = window_shift,
                     f = f,
                     threshold = threshold,
                     classes = classes,
                     fun = wecoma_fun,
                     na_action = wecoma_na_action)
    x = tibble::as_tibble(x)
  } else {
    nc = ncol(dimensions)
    nr = nrow(dimensions)
    yoffs = seq(1, nc, by = window_size)
    x = lapply(yoffs,
               FUN = get_motifels_single_proxy,
               x_path = x,
               type = type2,
               directions = directions,
               window_size = window_size,
               window_shift = window_shift,
               f = f,
               threshold = threshold,
               classes = classes,
               wecoma_fun = wecoma_fun,
               wecoma_na_action = wecoma_na_action,
               dimensions = dimensions)
    x = merge_and_update(x, window_size, nr)
  }
  x = convert_signatures(x, type = type,
                         ordered = ordered, repeated = repeated,
                         normalization = normalization)
  return(x)
}

get_polygons_single_proxy = function(window_id, x, window, ...){
  # print(window_id)
  result = lsp_signature(stars::st_as_stars(x[window[window_id, ]]), ...)
  if (nrow(result) == 1){
    result$id = window[[window_id, 1]]
    result$na_prop = NA
  }
  return(result)
}

get_polygons_all = function(x, type, directions, window,
                            f, threshold, classes,
                            ordered, repeated, normalization,
                            wecoma_fun, wecoma_na_action){
  type2 = prepare_type(type)
  if (inherits(x, "stars_proxy")){
    warning("Current implementation can be slow")
    window_ids = seq_len(nrow(window))
    threshold = 1
    x = lapply(window_ids,
               get_polygons_single_proxy,
               x = x,
               type = type2,
               window = window,
               #window_size = NULL,
               #window_shift = NULL,
               neighbourhood = c(directions),
               threshold = threshold,
               ordered = ordered,
               repeated = repeated,
               normalization = normalization,
               wecoma_fun = wecoma_fun,
               wecoma_na_action = wecoma_na_action,
               classes = classes)
    x = do.call(rbind, x)
  } else {
    x = get_polygons(x,
                     type = type2,
                     m = window,
                     directions = directions,
                     f = f,
                     threshold = threshold,
                     fun = wecoma_fun,
                     na_action = wecoma_na_action,
                     classes = classes)
    x = tibble::as_tibble(x)
  }
  x = convert_signatures(x, type, ordered, repeated, normalization)
  x
}
