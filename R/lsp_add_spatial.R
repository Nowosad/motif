#' Title
#'
#' @param x
#'
#' @param window
#' @param window_size
#' @param window_shift
#'
#' @examples
#' library(stars)
#' landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' plot(landform)
#' landform_lsp = lsp_add_spatial(landform, window_size = 100)
#' plot(landform_lsp)
#'
#' lc_cove = lsp_thumbprint(landform, type = "cove", window_size = 200, normalization = "pdf")
#' lc_cove_lsp = lsp_add_spatial(lc_cove)
#' plot(lc_cove_lsp)
#' plot(lc_cove_lsp["na_prop"])
#'
#' @aliases lsp_add_spatial
#' @rdname lsp_add_spatial
#'
#' @export
lsp_add_spatial = function(x = NULL, window = NULL, window_size = NULL, window_shift = NULL) UseMethod("lsp_add_spatial")

#' @name lsp_add_spatial
#' @export
lsp_add_spatial.default = function(x = NULL, window = NULL, window_size = NULL, window_shift = NULL){

  if (!missing(window_size) && !is.null(window_size)){
    x_crs = st_crs(x)
    x_nrow = nrow(x)
    x_ncol = ncol(x)

    x_bb = sf::st_bbox(x)

    x_delta_row = st_dimensions(x)[[1]][["delta"]]
    x_delta_col = st_dimensions(x)[[2]][["delta"]]

    if (is.null(window_shift)){
      window_shift = window_size
    }

    output = lsp_create_grid(x_crs = x_crs, x_bb = x_bb,
                             x_delta_row = x_delta_row, x_delta_col = x_delta_col,
                             window_shift = window_shift)

    # df_ids = get_motifels_ids(x_nrow, x_ncol, size, shift)
    # output$col = df_ids[, 1]
    # output$row = df_ids[, 2]

    return(output)
  } else if (missing(window) || is.null(window)){
    x_bb = sf::st_bbox(x)
    x_crs = st_crs(x)

    output = st_as_stars(x_bb, nx = 1, ny = 1, values = 1)
    names(output) = "id"

    return(output)

  } else {
    names(window) = "id"
    return(window)
  }
}

#' @name lsp_add_spatial
#' @export
lsp_add_spatial.lsp = function(x = NULL, window = NULL, window_size = NULL, window_shift = NULL){
  metadata = attr(x, "metadata")

  output_stars = lsp_create_grid(x_crs = metadata$crs, x_bb = metadata$bb,
                           x_delta_row = metadata$delta_y, x_delta_col = metadata$delta_x,
                           window_shift = metadata$window_shift)

  true_dim = dim(output_stars$id)
  matched_ids = match(output_stars$id, x$id)
  # matched_ids = matched_ids[-1]

  for (i in 2:ncol(x)){
    selected_colnames = colnames(x)[i]
    selected_vals = x[[selected_colnames]][matched_ids]
    if (inherits(selected_vals, "list")){
      warning("Column ", selected_colnames, " is of a list class and will be dropped from the output object.",
      call. = FALSE)
    } else {
      output_stars[[selected_colnames]] = selected_vals
      dim(output_stars[[selected_colnames]]) = true_dim
    }
  }
  return(output_stars)
}


lsp_create_grid = function(x_crs, x_bb, x_delta_row, x_delta_col, window_shift){


  cellshift = c(window_shift * x_delta_row,
                window_shift * x_delta_col)

  output_n_row = ceiling(abs((x_bb["xmax"] - x_bb["xmin"]) / cellshift[1]))
  output_n_col = ceiling(abs((x_bb["ymin"] - x_bb["ymax"]) / cellshift[1]))

  new_xmax = x_bb["xmin"] + (output_n_row * cellshift[1])
  new_ymin = x_bb["ymax"] + (output_n_col * cellshift[2])

  output_bb = st_bbox(c(
    xmin = unname(x_bb["xmin"]),
    ymin = unname(new_ymin),
    xmax = unname(new_xmax),
    ymax = unname(x_bb["ymax"])
  ))

  output = st_as_stars(output_bb,
                       nx = output_n_row,
                       ny = output_n_col,
                       values = as.integer(seq_len(output_n_row * output_n_col)))
  output = st_set_crs(output, value = x_crs)
  names(output) = "id"

  return(output)
}