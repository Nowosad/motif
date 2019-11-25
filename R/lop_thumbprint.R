#' Title
#'
#' @param ...
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
#' library(comat)
#' library(raster)
#' landcover = raster(system.file("raster/landcover.tif", package = "lopata"))
#' plot(landcover)
#' landform = raster(system.file("raster/landform.tif", package = "lopata"))
#' plot(landform)
#' #npp = raster("npp.tif")
#' #plot(npp)
#' ecoregions = raster(system.file("raster/ecoregions.tif", package = "lopata"))
#' plot(ecoregions)
#'
#' lop_thumbprint(landcover, type = "coma", threshold = 0.9)
#' lop_thumbprint(landcover, type = "cove", threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "cocoma", threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "cocove", threshold = 0.9)
#' #lop_thumbprint(landcover, npp, type = "wecoma", threshold = 0.9)
#' #lop_thumbprint(landcover, npp, type = "wecove", threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "incoma", threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "incove", threshold = 0.9)
#' lop_thumbprint(list(landcover, landform), type = "incove", threshold = 0.9)
#'
#' lop_thumbprint(landcover, type = "coma", window_size = 100, window_shift = 100, threshold = 0.9)
#' lop_thumbprint(landcover, type = "cove", window_size = 100, window_shift = 100, threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "cocoma", window_size = 100, window_shift = 100, threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "cocove", window_size = 100, window_shift = 100, threshold = 0.9)
#' #lop_thumbprint(landcover, npp, type = "wecoma", window_size = 100, window_shift = 100, threshold = 0.9)
#' #lop_thumbprint(landcover, npp, type = "wecove", window_size = 100, window_shift = 100, threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "incoma", window_size = 100, window_shift = 100, threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "incove", window_size = 100, window_shift = 100, threshold = 0.9)
#' lop_thumbprint(list(landcover, landform), type = "incove", window_size = 100, window_shift = 100, threshold = 0.9)
#'
#' lop_thumbprint(landcover, type = "coma", window = ecoregions, threshold = 0.9)
#' lop_thumbprint(landcover, type = "cove", window = ecoregions, threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "cocoma", window = ecoregions, threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "cocove", window = ecoregions, threshold = 0.9)
#' #lop_thumbprint(landcover, npp, type = "wecoma", window = ecoregions, threshold = 0.9)
#' #lop_thumbprint(landcover, npp, type = "wecove", window = ecoregions, threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "incoma", window = ecoregions, threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "incove", window = ecoregions, threshold = 0.9)
#' lop_thumbprint(list(landcover, landform), type = "incove", window = ecoregions, threshold = 0.9)
lop_thumbprint = function(..., type, window = NULL, window_size = NULL, window_shift = NULL,
                          neighbourhood = 4, threshold = 0.5, ordered = TRUE, repeated = TRUE,
                          normalization = "none", wecoma_fun = "mean", wecoma_na_action = "replace"){

  x = list(...)
  if (length(x) == 1 && is.list(x[[1]])){
    x = x[[1]]
  }

  x = lapply(x, raster::as.matrix)

  directions = as.matrix(neighbourhood)

  if (missing(window) || is.null(window)){
    if (is.null(window_size)){
      window_size = 0
    }
    if (missing(window_shift) || is.null(window_shift)){
      window_shift = window_size
    }
  }

  if (missing(window) || is.null(window)){
    if (type == "coma" || type == "cove"){
      x = get_motifels_coma(x[[1]],
                            directions = directions,
                            size = window_size,
                            shift = window_shift,
                            threshold = threshold)
    } else if (type == "cocoma" || type == "cocove"){
      x = get_motifels_cocoma(x[[1]],
                              x[[2]],
                              directions = directions,
                              size = window_size,
                              shift = window_shift,
                              threshold = threshold)
    } else if (type == "wecoma" || type == "wecove"){
      x = get_motifels_wecoma(x = x[[1]],
                              w = x[[2]],
                              directions = directions,
                              size = window_size,
                              shift = window_shift,
                              threshold = threshold,
                              fun = wecoma_fun,
                              na_action = wecoma_na_action)
    } else if (type == "incoma" || type == "incove"){
      x = get_motifels_incoma(x,
                              directions = directions,
                              size = window_size,
                              shift = window_shift,
                              threshold = threshold)
    }
  } else {
    window = raster::as.matrix(window)

    if (type == "coma" || type == "cove"){
      x = get_polygons_coma(x[[1]],
                            directions = directions,
                            m = window,
                            threshold = threshold)
    } else if (type == "cocoma" || type == "cocove"){
      x = get_polygons_cocoma(x[[1]],
                              x[[2]],
                              directions = directions,
                              m = window,
                              threshold = threshold)
    } else if (type == "wecoma" || type == "wecove"){
      x = get_polygons_wecoma(x = x[[1]],
                              w = x[[2]],
                              directions = directions,
                              m = window,
                              threshold = threshold,
                              fun = wecoma_fun,
                              na_action = wecoma_na_action)
    } else if (type == "incoma" || type == "incove"){
      x = get_polygons_incoma(x,
                              directions = directions,
                              m = window,
                              threshold = threshold)
    }
  }

  x = tibble::as_tibble(x)

  if (type == "cove"){
    x$matrix = lapply(x$matrix,
               comat::get_cove,
               ordered = ordered,
               normalization = normalization)
  } else if (type == "cocove"){
    x$matrix = lapply(x$matrix,
                      comat::get_cocove,
                      ordered = ordered,
                      normalization = normalization)
  } else if (type == "wecove"){
    x$matrix = lapply(x$matrix,
                      comat::get_wecove,
                      ordered = ordered,
                      normalization = normalization)
  } else if (type == "incove"){
    x$matrix = lapply(x$matrix,
                      comat::get_incove,
                      ordered = ordered,
                      repeated = repeated,
                      normalization = normalization
    )
  }

  return(x)
}
