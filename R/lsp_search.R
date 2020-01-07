#' Title
#'
#' @param x
#' @param y
#' @param type
#' @param dist_fun
#' @param window
#' @param window_size
#' @param window_shift
#' @param neighbourhood
#' @param threshold
#' @param ordered
#' @param repeated
#' @param normalization
#' @param wecoma_fun
#' @param wecoma_na_action
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(stars)
#'
#' lc = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#' #lf = read_stars(system.file("raster/landform.tif", package = "motif"))
#'
#' ext = st_bbox(c(xmin = -249797.344531127, xmax = -211162.693944285,
#'                 ymin = -597280.143035389, ymax = -558645.492448547),
#'                 crs = st_crs(lc))
#'
#' #ecoregions = read_stars(system.file("raster/ecoregions.tif", package = "motif"))
#' #plot(ecoregions)
#'
#' lc_ext = lc[ext]
#' #lf_ext = lf[ext]
#'
#' s1 = lsp_search(lc_ext, lc, type = "cove",
#'   dist_fun = "jensen-shannon", threshold = 0.9)
#' #s2 = lsp_search(c(lc_ext, lf_ext), c(lc, lf), type = "cocove",
#' #  dist_fun = "jensen-shannon", threshold = 0.9)
#' #s3 = lsp_search(c(lc_ext, lf_ext), c(lc, lf), type = "wecove",
#' #  dist_fun = "jensen-shannon", threshold = 0.9)
#' #s4 = lsp_search(c(lc_ext, lf_ext), c(lc, lf), type = "incove",
#' #  dist_fun = "jensen-shannon", threshold = 0.9)
#' #s5 = lsp_search(lf_ext, lf, type = "cove",
#' #  dist_fun = "jensen-shannon", threshold = 0.5, window_size = 250)
#' #s6 = lsp_search(lc_ext, lc, type = "cove",
#' #  dist_fun = "jensen-shannon", threshold = 0.5, window = ecoregions)
lsp_search = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                      neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                      normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace", ...) UseMethod("lsp_search")


#'
#' @name lsp_search
#' @export
lsp_search.stars = function(x, y, type, dist_fun, window = NULL, window_size = NULL, window_shift = NULL,
                      neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                      normalization = "pdf", wecoma_fun = "mean", wecoma_na_action = "replace", ...){

  x_metadata = stars::st_dimensions(x)
  y_metadata = stars::st_dimensions(y)

  x = lapply(x, function(x) `mode<-`(x, "integer"))
  classes_x = lapply(x, get_unique_values, TRUE)

  if (inherits(y, "stars_proxy")){
    classes_y = get_unique_values_proxy(y,
                                        ifelse(is.null(window_size), ceiling(nrow(y) / nrow(window)), window_size),
                                        nrow(y), ncol(y))
  } else {
    y = lapply(y, function(x) `mode<-`(x, "integer"))

    y = stars::st_as_stars(y)
    attr(y, "dimensions") = y_metadata

    classes_y = lapply(y, get_unique_values, TRUE)
  }

  classes = mapply(c, classes_x, classes_y, SIMPLIFY = FALSE)
  classes = lapply(classes, unique)
  classes = lapply(classes, sort)

  output = lsp_thumbprint(
    x = y,
    type = type,
    window = window,
    window_size = window_size,
    window_shift = window_shift,
    neighbourhood = neighbourhood,
    threshold = threshold,
    ordered = ordered,
    repeated = repeated,
    normalization = normalization,
    wecoma_fun = wecoma_fun,
    wecoma_na_action = wecoma_na_action,
    classes = classes
  )

  # unique_classes_all = attributes(output)[["metadata"]][["vals"]]

  if (type == "coma" || type == "cove") {
    input_thumbprint = comat::get_cove(
      comat::get_coma(x[[1]], neighbourhood = neighbourhood, classes = classes),
      ordered = ordered,
      normalization = normalization
    )
  } else if (type == "cocoma" || type == "cocove") {
    input_thumbprint = comat::get_cocove(
      comat::get_cocoma(x[[1]], x[[2]], neighbourhood = neighbourhood, classes = classes),
      ordered = ordered,
      normalization = normalization
    )
  } else if (type == "wecoma" || type == "wecove") {
    input_thumbprint = comat::get_wecove(
      comat::get_wecoma(
        x[[1]],
        x[[2]],
        neighbourhood = neighbourhood,
        classes = classes,
        fun = wecoma_fun,
        na_action = wecoma_na_action
      ),
      ordered = ordered,
      normalization = normalization
    )
  } else if (type == "incoma" || type == "incove") {
    input_thumbprint = comat::get_incove(
      comat::get_incoma(x, neighbourhood = neighbourhood, classes = classes),
      ordered = ordered,
      repeated = repeated,
      normalization = normalization
    )
  }

  unit = "log2"

  output$dist = unlist(lapply(
    output$signature,
    distance2,
    P = input_thumbprint,
    method = dist_fun,
    unit = unit,
    ...
  ))

  message("Metric: '", dist_fun, "' using unit: '", unit, "'.")

  output$signature = NULL

  output_stars = lsp_add_stars(y_metadata,
                                 window = window,
                                 window_size = window_size, window_shift = window_shift)

  # output_stars$na_prop = NA
  # output_stars$na_prop[which(output_stars$id %in% output$id)] = output$na_prop
  output_stars$na_prop = output$na_prop[match(output_stars$id, output$id)]

  # output_stars$dist = NA
  # output_stars$dist[which(output_stars$id %in% output$id)] = output$dist
  output_stars$dist = output$dist[match(output_stars$id, output$id)]

  return(output_stars)
}
