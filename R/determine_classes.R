#' Determine unique classes (internal function)
#'
#' @param x - a `stars` or `stars_proxy` object
#' @param window - a windows argument from `lsp_signature()`, `lsp_search()`, or `lsp_compare()`
#'
#' @return A list with vector of numbers (unique classes)
determine_classes = function(x, window){
  if (inherits(x, "stars_proxy")){
    if (missing(window) || is.null(window)){
      if (is.null(window)){
        window_size = 0
      }
    } else if (is.numeric(window)){
      window_size = window
    } else {
      window_size = NULL
    }
    nr_elements = ifelse(nrow(window) < 50, 50, nrow(window))
    classes = get_unique_values_proxy(x,
                                       ifelse(is.null(window_size),
                                              ceiling(nrow(x) / nr_elements),
                                              window_size))
  } else {
    classes = lapply(x, get_unique_values, TRUE)
  }
}

get_unique_values_proxy = function(x, window_size){
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
                      nXSize = unname(nr),
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
