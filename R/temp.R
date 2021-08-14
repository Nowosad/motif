# remove when/if https://github.com/r-spatial/stars/pull/444/files is accepted
st_as_terra2 = function(x, ...) {
  stopifnot(inherits(x, "stars"))
  st_upfront = function(x, first = attr(stars::st_dimensions(x), "raster")$dimensions) {
    if (!is.character(first))
      first = names(stars::st_dimensions(x))[first]
    if (!any(is.na(first)))
      aperm(x, c(first, setdiff(names(stars::st_dimensions(x)), first)))
    else
      x
  }
  x = st_upfront(x) # x/y dimensions first
  if (length(dim(x)) > 3) {
    warning("folding all higher dimensions into the third dimension") # nocov
    x = stars::st_apply(x, 1:2, as.vector) # fortunes::fortune("side effect") # nocov
  }
  if (length(dim(x)) == 2 && length(x) > 1)
    x = merge(x)
  d = stars::st_dimensions(x)
  if (d[[2]]$delta > 0) { # swap:
    ny = dim(x)[2]
    d[[2]]$offset = d[[2]]$offset + ny * d[[2]]$delta # top
    d[[2]]$delta = -d[[2]]$delta # going down
    x[[1]] = if (length(dim(x)) == 2)
      x[[1]][,ny:1]
    else
      x[[1]][,ny:1,]
  }
  dxy = attr(d, "raster")$dimensions
  stopifnot(all(dxy %in% names(d)))
  bb = sf::st_bbox(x)
  values = if (is.factor(x[[1]])) {
    structure(x[[1]], dim = NULL)
  } else {
    as.vector(x[[1]]) # would convert factor into character
  }
  third = setdiff(names(d), dxy)
  b = terra::rast(nrows = dim(x)[ dxy[2] ], ncols=dim(x)[ dxy[1] ],
                  xmin = bb[1], xmax = bb[3], ymin = bb[2], ymax = bb[4],
                  nlyrs = ifelse(length(dim(x)) == 2, 1, dim(x)[third]),
                  crs = sf::st_crs(x)$wkt)
  terra::values(b) = values
  if (any(unlist(lapply(x, is.factor)))){
    terra::coltab(b) = lapply(x, function(x) t(grDevices::col2rgb(attr(x, "colors"), alpha = TRUE)))
  }
  if (length(dim(x)) != 2){
    z = seq(d[[third]])
    if (!any(is.na(z))) {
      if (is.character(z)) {
        names(b) = z
      } else {
        names(b) = paste0(third, z)
      }
    }
  }
  b
}
