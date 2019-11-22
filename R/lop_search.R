
lop_search = function(x, y, type, normalization = "pdf", dist_fun = jensen_shannon){

  output = lop_thumbprint(y, type = type, threshold = 0.9, normalization = normalization)

  unique_classes_all = attributes(output)[["metadata"]][["vals"]]

  x = lapply(x, raster::as.matrix)

  if (type == "coma" || type == "cove"){
    input_thumbprint = get_cove(get_coma(x[[1]], classes = unique_classes_all), normalization = normalization)
  } else if (type == "cocoma" || type == "cocove"){
    input_thumbprint = get_cocove(get_cocoma(x[[1]], x[[2]], classes = unique_classes_all), normalization = normalization)
  } else if (type == "wecoma" || type == "wecove"){
    input_thumbprint = get_wecove(get_wecoma(x[[1]], x[[2]], classes = unique_classes_all), normalization = normalization)
  } else if (type == "incoma" || type == "incove"){
    input_thumbprint = get_incove(get_incoma(x, classes = unique_classes_all), normalization = normalization)
  }

  output$dist = unlist(lapply(output$matrix, dist_fun,
                              P = input_thumbprint, testNA = FALSE,
                              unit = "log2"))
  return(output)
}


library(lopata)
library(raster)
library(philentropy)
ext = extent(c(xmin = -249797.344531127, xmax = -211162.693944285,
               ymin = -597280.143035389, ymax = -558645.492448547))

lc = raster("inst/raster/landcover.tif")
lf = raster("inst/raster/landform.tif")

lc_ext = crop(lc, ext)
lf_ext = crop(lf, ext)


s1 = lop_search(list(lc_ext), list(lc), type = "cove", dist_fun = jensen_shannon)
s2 = lop_search(list(lc_ext, lf_ext), list(lc, lf), type = "cocove", dist_fun = jensen_shannon)
s3 = lop_search(list(lc_ext, lf_ext), list(lc, lf), type = "wecove", dist_fun = jensen_shannon)
s4 = lop_search(list(lc_ext, lf_ext), list(lc, lf), type = "incove", dist_fun = jensen_shannon)
