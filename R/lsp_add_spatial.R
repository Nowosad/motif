#' Creates or adds a stars object
#'
#' Creates or adds a stars object based on the input object or a set of parameters.
#' It accepts either an object of class `stars` or `lsp`.
#' In the first case, the output is created based on
#' the `window` parameter.
#' In the second case, the output converts the `lsp` object into
#' a `stars` object.
#'
#' @param x Object of class `stars` or `lsp`.
#' For `stars`, `window` or `window_size` can be used.
#' @param window Specifies areas for analysis. It can be either: `NULL`, a numeric value, or an `sf` object. If `window=NULL` calculations are performed for a whole area. If the `window` argument is numeric, it is a length of the side of a square-shaped block of cells. Expressed in the numbers of cells, it defines the extent of a local pattern. If an `sf` object is provided, each feature (row) defines the extent of a local pattern. The `sf` object should have one attribute (otherwise, the first attribute is used as an id).
#'
#' @return A `stars` object converted from the input object or a provided set of parameters
#'
#' @examples
#' library(stars)
#' landform = read_stars(system.file("raster/landforms.tif", package = "motif"))
#' plot(landform)
#' landform_lsp = lsp_add_stars(landform, window = 100)
#' plot(landform_lsp)
#'
#' lc_cove = lsp_signature(landform, type = "cove", window = 200, normalization = "pdf")
#' lc_cove_lsp = lsp_add_stars(lc_cove)
#' plot(lc_cove_lsp)
#' plot(lc_cove_lsp["na_prop"])
#'
#'
#' \donttest{
#' # larger data example
#' library(stars)
#' landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' plot(landform)
#' landform_lsp = lsp_add_stars(landform, window = 100)
#' plot(landform_lsp)
#'
#' lc_cove = lsp_signature(landform, type = "cove", window = 200, normalization = "pdf")
#' lc_cove_lsp = lsp_add_stars(lc_cove)
#' plot(lc_cove_lsp)
#' plot(lc_cove_lsp["na_prop"])
#' }
#' @aliases lsp_add_stars
#' @rdname lsp_add_stars
#'
#' @export
lsp_add_stars = function(x = NULL, window = NULL) UseMethod("lsp_add_stars")

#' @name lsp_add_stars
#' @export
lsp_add_stars.default = function(x = NULL, window = NULL){

  if (length(window) == 2){
    window_shift = window[2]
    window = window[1]
  } else if (length(window) == 1){
    window = window[1]
    window_shift = window[1]
  }

  if (is.numeric(window) && window != 0){
    x_crs = sf::st_crs(x)
    x_bb = sf::st_bbox(x)
    x_delta_row = stars::st_dimensions(x)[[1]][["delta"]]
    x_delta_col = stars::st_dimensions(x)[[2]][["delta"]]

    output = lsp_create_grid(x_crs = x_crs,
                             x_bb = x_bb,
                             x_delta_row = x_delta_row,
                             x_delta_col = x_delta_col,
                             window_shift = window_shift)
    return(output)

  } else if (is.null(window)){
    x_bb = sf::st_bbox(x)

    output = stars::st_as_stars(x_bb, nx = 1, ny = 1, values = 1)
    names(output) = "id"

    return(output)

  } else {
    x_delta_row = stars::st_dimensions(x)[[1]][["delta"]]
    x_delta_col = stars::st_dimensions(x)[[2]][["delta"]]

    window = stars::st_rasterize(window,
                                 template = stars::st_as_stars(sf::st_bbox(x),
                                                               values = NA_integer_,
                                                               dx = unname(x_delta_row),
                                                               dy = unname(x_delta_col)))
    names(window) = "id"
    return(window)
  }
}

#' @name lsp_add_stars
#' @export
lsp_add_stars.lsp = function(x = NULL, window = NULL){
  metadata = attr(x, "metadata")
  if (metadata$use_window && is.null(window)){
    stop("This function requires an sf object in the window argument for irregular local landscapes.", call. = FALSE)
  }

  if (is.null(window)){
    output_stars = lsp_create_grid(x_crs = metadata$crs,
                                   x_bb = metadata$bb,
                                   x_delta_row = metadata$delta_y,
                                   x_delta_col = metadata$delta_x,
                                   window_shift = metadata$window_shift)
  } else {
    output_stars = stars::st_rasterize(window[1],
                                 template = stars::st_as_stars(metadata$bb,
                                                               values = NA_integer_,
                                                               dx = metadata$delta_y,
                                                               dy = metadata$delta_x))
  }
  x = lsp_restructure(x)
  output_stars = join_stars(output_stars, x, by = "id")

  return(output_stars)
}

join_stars = function(stars, df, by){
  true_dim = dim(stars[[by]])
  matched_ids = match(stars[[by]], df[[by]])
  # matched_ids = matched_ids[-1]

  nonid_colnames = colnames(df)[-which(colnames(df) == by)]

  for (i in seq_along(nonid_colnames)){
    selected_colnames = nonid_colnames[i]
    selected_vals = df[[selected_colnames]][matched_ids]
    if (inherits(selected_vals, "list")){
      warning("Column ", selected_colnames, " is of a list class and will be dropped from the output object.",
              call. = FALSE)
    } else {
      stars[[selected_colnames]] = selected_vals
      dim(stars[[selected_colnames]]) = true_dim
    }
  }
  return(stars)
}


lsp_create_grid = function(x_crs, x_bb, x_delta_row, x_delta_col, window_shift){

  cellshift = c(window_shift * x_delta_row,
                window_shift * x_delta_col)

  output_n_row = ceiling2(abs((x_bb["xmax"] - x_bb["xmin"]) / cellshift[1]))
  output_n_col = ceiling2(abs((x_bb["ymin"] - x_bb["ymax"]) / cellshift[2]))

  new_xmax = x_bb["xmin"] + (output_n_row * cellshift[1])
  new_ymin = x_bb["ymax"] + (output_n_col * cellshift[2])

  output_bb = sf::st_bbox(c(
    xmin = unname(x_bb["xmin"]),
    ymin = unname(new_ymin),
    xmax = unname(new_xmax),
    ymax = unname(x_bb["ymax"])
  ))

  output = stars::st_as_stars(output_bb,
                       nx = unname(output_n_row),
                       ny = unname(output_n_col),
                       values = as.integer(seq_len(output_n_row * output_n_col)))

  output = sf::st_set_crs(output, value = x_crs)
  names(output) = "id"

  return(output)
}

#' Creates or adds a terra object
#'
#' Creates or adds a terra object based on the input object or a set of parameters.
#' It accepts either an object of class `stars` or `lsp`.
#' In the first case, the output is created based on
#' the `window` parameter.
#' In the second case, the output converts the `lsp` object into
#' a `terra` object.
#'
#' @param x Object of class `stars` or `lsp`.
#' For `stars`, `window` or `window_size` can be used.
#' @param window Specifies areas for analysis. It can be either: `NULL`, a numeric value, or an `sf` object. If `window=NULL` calculations are performed for a whole area. If the `window` argument is numeric, it is a length of the side of a square-shaped block of cells. Expressed in the numbers of cells, it defines the extent of a local pattern. If an `sf` object is provided, each feature (row) defines the extent of a local pattern. The `sf` object should have one attribute (otherwise, the first attribute is used as an id).
#'
#' @return A `terra` object converted from the input object or a provided set of parameters
#'
#' @examples
#' library(stars)
#' library(terra)
#' landform = read_stars(system.file("raster/landforms.tif", package = "motif"))
#' #plot(landform)
#' landform_lsp = lsp_add_terra(landform, window = 100)
#' #plot(landform_lsp)
#'
#' #lc_cove = lsp_signature(landform, type = "cove", window = 200, normalization = "pdf")
#' #lc_cove_lsp = lsp_add_terra(lc_cove)
#' #plot(lc_cove_lsp)
#' #plot(lc_cove_lsp["na_prop"])
#'
#' @export
lsp_add_terra = function(x = NULL, window = NULL){
  if (!requireNamespace("terra", quietly = TRUE)){
    stop("package terra required, please install it first") # nocov
  }
  output = lsp_add_stars(x = x, window = window)
  output_names = names(output)
  output = terra::rast(output)
  names(output) = output_names
  return(output)
}

#' Creates or adds a sf object
#'
#' Creates or adds a sf object based on the input object or a set of parameters.
#' It accepts either an object of class `stars` or `lsp`.
#' In the first case, the output is created based on
#' a set of parameters (`window_size` and `window_shift` or `window`).
#' In the second case, the output converts the `lsp` object into
#' a `sf` object.
#'
#' @param x Object of class `stars` or `lsp`.
#' For `stars`, `window` or `window_size` can be used.
#' @param window Specifies areas for analysis. It can be either: `NULL`, a numeric value, or an `sf` object. If `window=NULL` calculations are performed for a whole area. If the `window` argument is numeric, it is a length of the side of a square-shaped block of cells. Expressed in the numbers of cells, it defines the extent of a local pattern. If an `sf` object is provided, each feature (row) defines the extent of a local pattern. The `sf` object should have one attribute (otherwise, the first attribute is used as an id).
#'
#' @return An `sf` object converted from the input object or a provided set of parameters
#'
#' @examples
#' library(stars)
#' landform = read_stars(system.file("raster/landforms.tif", package = "motif"))
#' plot(landform)
#' landform_lsp = lsp_add_sf(landform, window = 100)
#' plot(landform_lsp)
#'
#' lc_cove = lsp_signature(landform, type = "cove", window = 200, normalization = "pdf")
#' lc_cove_lsp = lsp_add_sf(lc_cove)
#' plot(lc_cove_lsp["id"])
#' plot(lc_cove_lsp["na_prop"])
#'
#' \donttest{
#' # larger data example
#' library(stars)
#' landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' plot(landform)
#' landform_lsp = lsp_add_sf(landform, window = 100)
#' plot(landform_lsp)
#'
#' lc_cove = lsp_signature(landform, type = "cove", window = 200, normalization = "pdf")
#' lc_cove_lsp = lsp_add_sf(lc_cove)
#' plot(lc_cove_lsp["id"])
#' plot(lc_cove_lsp["na_prop"])
#' }
#' @aliases lsp_add_sf
#' @rdname lsp_add_sf
#'
#' @export
lsp_add_sf = function(x = NULL, window = NULL) UseMethod("lsp_add_sf")

#' @name lsp_add_sf
#' @export
lsp_add_sf.default = function(x = NULL, window = NULL){

  if (length(window) == 2){
    window_shift = window[2]
    window = window[1]
  } else if (length(window) == 1){
    window = window[1]
    window_shift = window[1]
  }

  if (is.numeric(window) && window != 0){
    if (inherits(x, "SpatRaster")){
      x = stars::st_as_stars(x)
    }
    x_crs = sf::st_crs(x)
    x_bb = sf::st_bbox(x)
    x_delta_row = stars::st_dimensions(x)[[1]][["delta"]]
    x_delta_col = stars::st_dimensions(x)[[2]][["delta"]]

    output = lsp_create_grid(x_crs = x_crs,
                             x_bb = x_bb,
                             x_delta_row = x_delta_row,
                             x_delta_col = x_delta_col,
                             window_shift = window_shift)

    output = sf::st_as_sf(output)
    return(output)

  } else if (is.null(window)){
    x_bb = sf::st_bbox(x)
    x_crs = sf::st_crs(x)

    output = stars::st_as_stars(x_bb, nx = 1, ny = 1, values = 1)
    names(output) = "id"

    output = sf::st_as_sf(output)

    return(output)

  } else {

    # names(window) = "id"
    return(window)
  }
}

#' @name lsp_add_sf
#' @export
lsp_add_sf.lsp = function(x = NULL, window = NULL){
  metadata = attr(x, "metadata")
  if (metadata$use_window && is.null(window)){
    stop("This function requires an sf object in the window argument for irregular local landscapes.", call. = FALSE)
  }
  if (is.null(window)){
    output_stars = lsp_create_grid(x_crs = metadata$crs,
                                   x_bb = metadata$bb,
                                   x_delta_row = metadata$delta_y,
                                   x_delta_col = metadata$delta_x,
                                   window_shift = metadata$window_shift)

    output_sf = sf::st_as_sf(output_stars)
  } else {
    output_sf = window
  }
  output_sf = merge(x, output_sf, by = "id", all.x = TRUE)
  output_sf = tibble::as_tibble(output_sf)
  output_sf = sf::st_as_sf(output_sf)

  return(output_sf)
}
