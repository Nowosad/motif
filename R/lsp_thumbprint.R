#' Title
#'
#' @param x
#' @param type
#' @param neighbourhood
#' @param window_size
#' @param window_shift
#' @param threshold
#' @param ordered
#' @param repeated
#' @param normalization
#' @param fun
#' @param na_action
#'
#' @return
#' @export
#'
#' @examples
#' library(stars)
#'
#' landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
#' #plot(landcover)
#' #landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' #plot(landform)
#' #ecoregions = read_stars(system.file("raster/ecoregions.tif", package = "motif"))
#' #plot(ecoregions)
#'
#' lsp_thumbprint(landcover, type = "coma", threshold = 0.9)
#' #lsp_thumbprint(landcover, type = "cove", threshold = 0.9)
#' #lsp_thumbprint(landcover, type = "cove", threshold = 0.9, classes = 10)
#' #lsp_thumbprint(c(landcover, landform), type = "incove", threshold = 0.9)
#'
#' #lsp_thumbprint(landcover, type = "coma", window_size = 100, window_shift = 100, threshold = 0.9)
#'
#' #lsp_thumbprint(landcover, type = "coma", window = ecoregions, threshold = 0.9)
lsp_thumbprint = function(x, type, window = NULL, window_size = NULL, window_shift = NULL,
                          neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                          normalization = "none", wecoma_fun = "mean", wecoma_na_action = "replace",
                          classes = NULL){

  # x = c(landcover, landform)

  x_crs = sf::st_crs(x)
  x_bb = sf::st_bbox(x)
  x_delta_row = stars::st_dimensions(x)[[1]][["delta"]]
  x_delta_col = stars::st_dimensions(x)[[2]][["delta"]]
  # attr_x = attributes(x)

  x = lapply(x, function(x) `mode<-`(x, "integer"))

  # attributes(x) = attr_x

  if (is.null(classes)){
    classes = lapply(x, get_unique_values, TRUE)
  }
  if (inherits(classes, "integer")){
    classes = list(classes)
  }

  directions = as.matrix(neighbourhood)

  if (missing(window) || is.null(window)){
    if (is.null(window_size)){
      window_size = 0
    }
    if (missing(window_shift) || is.null(window_shift)){
      window_shift = window_size
    }
  } else {
    window = lapply(window, function(x) `mode<-`(x, "integer"))
  }

  if (missing(window) || is.null(window)){
    if (is.function(type)){
      x = get_motifels_fun(x,
                           size = window_size,
                           shift = window_shift,
                           f = type,
                           threshold = threshold,
                           classes = classes)
    } else if (type == "coma" || type == "cove"){
      x = get_motifels_coma(x[[1]],
                            directions = directions,
                            size = window_size,
                            shift = window_shift,
                            threshold = threshold,
                            classes = classes)
    } else if (type == "cocoma" || type == "cocove"){
      x = get_motifels_cocoma(x[[1]],
                              x[[2]],
                              directions = directions,
                              size = window_size,
                              shift = window_shift,
                              threshold = threshold,
                              classes = classes)
    } else if (type == "wecoma" || type == "wecove"){
      x = get_motifels_wecoma(x = x[[1]],
                              w = x[[2]],
                              directions = directions,
                              size = window_size,
                              shift = window_shift,
                              threshold = threshold,
                              classes = classes,
                              fun = wecoma_fun,
                              na_action = wecoma_na_action)
    } else if (type == "incoma" || type == "incove"){
      x = get_motifels_incoma(x,
                              directions = directions,
                              size = window_size,
                              shift = window_shift,
                              threshold = threshold,
                              classes = classes)
    }
  } else {
    window = raster::as.matrix(window)

    if (is.function(type)){
      x = get_polygons_fun(x,
                           m = window[[1]],
                           f = type,
                           threshold = threshold,
                           classes = classes)
    } else if (type == "coma" || type == "cove"){
      x = get_polygons_coma(x[[1]],
                            directions = directions,
                            m = window[[1]],
                            threshold = threshold,
                            classes = classes)
    } else if (type == "cocoma" || type == "cocove"){
      x = get_polygons_cocoma(x[[1]],
                              x[[2]],
                              directions = directions,
                              m = window[[1]],
                              threshold = threshold,
                              classes = classes)
    } else if (type == "wecoma" || type == "wecove"){
      x = get_polygons_wecoma(x = x[[1]],
                              w = x[[2]],
                              directions = directions,
                              m = window[[1]],
                              threshold = threshold,
                              classes = classes,
                              fun = wecoma_fun,
                              na_action = wecoma_na_action)
    } else if (type == "incoma" || type == "incove"){
      x = get_polygons_incoma(x,
                              directions = directions,
                              m = window[[1]],
                              threshold = threshold,
                              classes = classes)
    }
  }

  x = tibble::as_tibble(x)

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
    }
  }

  # add spatial metadata
  attr(x, "metadata") = c(attr(x, "metadata"),
                          crs = list(x_crs),
                          bb = list(x_bb),
                          delta_x = x_delta_col,
                          delta_y = x_delta_row,
                          window_size = window_size,
                          window_shift = window_shift,
                          use_window = !missing(window))

  return(structure(x, class = c("lsp", class(x))))
}
