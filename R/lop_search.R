#' Title
#'
#' @param x
#' @param y
#' @param type
#' @param normalization
#' @param dist_fun
#'
#' @return
#' @export
#'
#' @examples
#' library(raster)
#' library(philentropy)
#' ext = extent(c(xmin = -249797.344531127, xmax = -211162.693944285,
#'                ymin = -597280.143035389, ymax = -558645.492448547))
#'
#' lc = raster(system.file("raster/landcover.tif", package = "lopata"))
#' lf = raster(system.file("raster/landform.tif", package = "lopata"))
#'
#' lc_ext = crop(lc, ext)
#' lf_ext = crop(lf, ext)
#'
#' s1 = lop_search(list(lc_ext), list(lc), type = "cove", dist_fun = jensen_shannon, threshold = 0.9)
#' s2 = lop_search(list(lc_ext, lf_ext), list(lc, lf), type = "cocove", dist_fun = jensen_shannon, threshold = 0.9)
#' s3 = lop_search(list(lc_ext, lf_ext), list(lc, lf), type = "wecove", dist_fun = jensen_shannon, threshold = 0.9)
#' s4 = lop_search(list(lc_ext, lf_ext), list(lc, lf), type = "incove", dist_fun = jensen_shannon, threshold = 0.9)
lop_search = function(x, y, type, dist_fun, neighbourhood = 4, size = NULL, shift = NULL,
                      threshold = 0.5, ordered = TRUE, repeated = TRUE,
                      normalization = "pdf", fun = "mean", na_action = "replace"){

  output = lop_thumbprint(y, type = type, neighbourhood = neighbourhood, size = size, shift = shift,
                          threshold = threshold, ordered = ordered, repeated = repeated,
                          normalization = normalization, fun = fun, na_action = na_action)

  unique_classes_all = attributes(output)[["metadata"]][["vals"]]

  x = lapply(x, raster::as.matrix)

  if (type == "coma" || type == "cove"){
    input_thumbprint = comat::get_cove(comat::get_coma(x[[1]], classes = unique_classes_all), normalization = normalization)
  } else if (type == "cocoma" || type == "cocove"){
    input_thumbprint = comat::get_cocove(comat::get_cocoma(x[[1]], x[[2]], classes = unique_classes_all), normalization = normalization)
  } else if (type == "wecoma" || type == "wecove"){
    input_thumbprint = comat::get_wecove(comat::get_wecoma(x[[1]], x[[2]], classes = unique_classes_all), normalization = normalization)
  } else if (type == "incoma" || type == "incove"){
    input_thumbprint = comat::get_incove(comat::get_incoma(x, classes = unique_classes_all), normalization = normalization)
  }

  output$dist = unlist(lapply(output$matrix, dist_fun,
                              P = input_thumbprint, testNA = FALSE,
                              unit = "log2"))
  output$matrix = NULL
  return(output)
}



