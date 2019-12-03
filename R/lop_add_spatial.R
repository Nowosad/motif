#' @examples
#' library(stars)
#' landcover = read_stars(system.file("raster/landcover2015.tif", package = "lopata"))
#' plot(landcover)
#' x = landcover
#' lop_add_spatial(landcover, window_size = 100)
#'
#'
#' # ?fill_na
#' landform = read_stars(system.file("raster/landform.tif", package = "lopata"))
#' my_fun = function(x) landscapemetrics::lsm_l_ent(x, neighbourhood = 4, base = "log2")[["value"]]
#' ee = lop_thumbprint(landform, type = my_fun, threshold = 0.2, window_size = 100)
#'
#' output$values2 = NA
#' output$values2[which(output$values %in% ee$id)] = unlist(ee$signature)
#' output$values2
#'
#' #write_stars(output["values2"], "aa.tif")
#'
#' plot(output["values2"])
#'
lop_add_spatial = function(x, window = NULL, window_size = NULL, window_shift = NULL){

  if (missing(window) || is.null(window)){
    x_crs = st_crs(x)
    x_nrow = nrow(x)
    x_ncol = ncol(x)

    if (is.null(window_shift)){
      window_shift = window_size
    }

    x_bb = sf::st_bbox(x)

    x_delta_row = st_dimensions(x)[[1]][["delta"]]
    x_delta_col = st_dimensions(x)[[2]][["delta"]]

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
    names(output) = "ids"

    # df_ids = get_motifels_ids(x_nrow, x_ncol, size, shift)
    # output$col = df_ids[, 1]
    # output$row = df_ids[, 2]

    return(output)
  } else {
    return(window)
  }
}
