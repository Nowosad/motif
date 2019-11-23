#' Title
#'
#' @param ...
#' @param type
#' @param neighbourhood
#' @param size
#' @param shift
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
#' lop_thumbprint(landcover, type = "coma", threshold = 0.9)
#' lop_thumbprint(landcover, type = "cove", threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "cocoma", threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "cocove", threshold = 0.9)
#' #lop_thumbprint(landcover, npp, type = "wecoma", threshold = 0.9)
#' #lop_thumbprint(landcover, npp, type = "wecove", threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "incoma", threshold = 0.9)
#' lop_thumbprint(landcover, landform, type = "incove", threshold = 0.9)
#' lop_thumbprint(list(landcover, landform), type = "incove", threshold = 0.9)
lop_thumbprint = function(..., type, neighbourhood = 4, size = NULL, shift = NULL,
                          threshold = 0.5, ordered = TRUE, repeated = TRUE,
                          normalization = "none", fun = "mean", na_action = "replace"){

  x = list(...)
  if (length(x) == 1 && is.list(x[[1]])){
    x = x[[1]]
  }

  x = lapply(x, raster::as.matrix)

  directions = as.matrix(neighbourhood)

  if (is.null(size)){
    size = 0
  }
  if (missing(shift) || is.null(shift)){
    shift = size
  }

  if (type == "coma" || type == "cove"){
    x = get_motifels_coma(x[[1]],
                          directions = directions,
                          size = size,
                          shift = shift,
                          threshold = threshold)
  } else if (type == "cocoma" || type == "cocove"){
    x = get_motifels_cocoma(x[[1]],
                            x[[2]],
                            directions = directions,
                            size = size,
                            shift = shift,
                            threshold = threshold)
  } else if (type == "wecoma" || type == "wecove"){
    x = get_motifels_wecoma(x = x[[1]],
                            w = x[[2]],
                            directions = directions,
                            size = size,
                            shift = shift,
                            threshold = threshold,
                            fun = fun,
                            na_action = na_action)
  } else if (type == "incoma" || type == "incove"){
    x = get_motifels_incoma(x,
                            directions = directions,
                            size = size,
                            shift = shift,
                            threshold = threshold)
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
