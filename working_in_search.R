library(raster)

# drawExtent(show=TRUE, col="red")
ext = extent(c(xmin = -249797.344531127, xmax = -211162.693944285,
             ymin = -597280.143035389, ymax = -558645.492448547))
input = crop(raster("inst/raster/landcover.tif"), ext)
plot(input)

input_coma = lop_coma(input)
input_cove = lop_cove(input_coma, normalization = "pdf")
input_cove$vector

comp = raster("inst/raster/landcover.tif")
plot(comp)


input_x = as.matrix(input)
comp_x = as.matrix(comp)
all_x = comat:::rcpp_get_coma_internal(input_x, directions = matrix(4), comat:::get_unique_values(comp_x, TRUE))
all_cove_x = comat::get_cove(all_x, normalization = "pdf")

library(lopata)
comp_coma = lop_coma(comp, size = 128)
# comp_coma$matrix

comp_cove = lop_cove(comp_coma, normalization = "pdf")
comp_cove$vector

library(philentropy)
xx = distance(do.call(rbind, comp_cove$vector))

my_grid = lop_grid(comp, size = 128)
comp_coma$dist = unlist(lapply(comp_cove$vector, jensen_shannon, P = all_cove_x, testNA = FALSE, unit =
              "log2"))


my_grid2 = dplyr::left_join(my_grid, comp_coma[c("row", "col", "dist")])

plot(my_grid2["dist"])


# A -----------------------------------------------------------------------
ext = extent(c(xmin = -249797.344531127, xmax = -211162.693944285,
               ymin = -597280.143035389, ymax = -558645.492448547))

lc = raster("inst/raster/landcover.tif")
lf = raster("inst/raster/landform.tif")

lc_ext = crop(lc, ext)
lf_ext = crop(lf, ext)

lclf_ext = list(as.matrix(lc_ext), as.matrix(lf_ext))
lclf = list(as.matrix(lc), as.matrix(lf))

lclf_unique_classes = lapply(lclf, comat:::get_unique_values, TRUE)
incoma_ext = comat:::rcpp_get_incoma_list(lclf_ext, directions = matrix(4), lclf_unique_classes)
incove_ext = comat:::rcpp_get_incove(incoma_ext, ordered = FALSE, repeated = FALSE, normalization = "pdf")

incoma = lop_incoma(stack(lc, lf), size = 128)
incove = lop_incove(incoma, ordered = FALSE, repeated = FALSE, normalization = "pdf")

my_grid = lop_grid(lc, size = 128)

incove$dist = unlist(lapply(incove$vector, jensen_shannon,
                            P = incove_ext, testNA = FALSE,
                            unit = "log2"))

my_grid2 = dplyr::left_join(my_grid, incove[c("row", "col", "dist")])

plot(my_grid2["dist"])
