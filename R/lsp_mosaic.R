#' Creates a raster mosaic
#'
#' Creates a raster mosaic by rearranging spatial data for example regions.
#' See examples.
#'
#' @param x Usually the output of the `lsp_add_examples()` function
#' @param output The class of the output. Either `"stars"` or `terra`
#'
#' @return A `stars` or `terra` object
#' @export
#'
#' @examples
#' \donttest{
#' # larger data example
#' library(stars)
#' library(sf)
#' landform = read_stars(system.file("raster/landform.tif", package = "motif"))
#' landform_cove = lsp_signature(landform,
#'                                type = "cove",
#'                                window = 200,
#'                                normalization = "pdf")
#'
#' landform_dist = lsp_to_dist(landform_cove,
#'                             dist_fun = "jensen-shannon")
#'
#' landform_hclust = hclust(landform_dist, method = "ward.D2")
#' plot(landform_hclust)
#'
#' clusters = cutree(landform_hclust, k = 6)
#'
#' landform_grid_sf = lsp_add_clusters(landform_cove, clusters)
#' plot(landform_grid_sf["clust"])
#'
#' landform_grid_sf_sel = landform_grid_sf %>%
#'     dplyr::filter(na_prop == 0) %>%
#'     dplyr::group_by(clust) %>%
#'     dplyr::slice_sample(n = 16, replace = TRUE)
#'
#' landform_grid_sf_sel = lsp_add_examples(x = landform_grid_sf_sel, y = landform)
#' landform_grid_sf_sel
#'
#' landform_clust_m = lsp_mosaic(landform_grid_sf_sel)
#'
#' plot(landform_clust_m)
#' }
lsp_mosaic = function(x, output = "stars"){
  if ("clust" %in% names(x)){
    unique_clust = unique(x$clust)
    all_mosaics = lapply(unique_clust, lsp_one_mosaic, x = x)
    all_mosaics = c(all_mosaics, along = "clust")
    all_mosaics = do.call(c, all_mosaics)
  } else {
    all_mosaics = lsp_one_mosaic(x)
  }
  if (output == "terra"){
    all_mosaics = terra::rast(all_mosaics)
  }
  return(all_mosaics)
}

lsp_one_mosaic = function(x, my_clust){

  r = x$region[[1]]

  if (!missing(my_clust)){
    sel_r = x[x$clust == my_clust, ]
  } else {
    sel_r = x
  }
  one_dim = sqrt(nrow(sel_r))

  new_mat = matrix(NA_integer_,
                   ncol = ncol(r) * one_dim,
                   nrow = nrow(r) * one_dim)

  new_list = lapply(r, create_new_matrix, one_dim)

  rast_number = 0

  for (i in seq_len(one_dim)) {
    for (j in seq_len(one_dim)) {
      rast_number = rast_number + 1
      # print(rast_number)

      val_mat = lapply(sel_r$region[[rast_number]], as.matrix)

      new_list = mapply(insert_matrix, val_mat, new_list,
                        starting_x = i, starting_y = j,
                        SIMPLIFY = FALSE)
    }
  }

  m = stars::st_as_stars(new_list)
  m = stars::st_set_dimensions(m, which = "X1", point = FALSE)
  m = stars::st_set_dimensions(m, which = "X2", point = FALSE)
  m = stars::st_set_dimensions(m, which = "X1", xy = c("X1", "X2"))

  attr(m, "dimensions")[[1]]$offset = 0
  attr(m, "dimensions")[[1]]$delta = 1
  attr(m, "dimensions")[[2]]$offset = 0
  attr(m, "dimensions")[[2]]$delta = 1

  m
}

create_new_matrix = function(r, one_dim){
  new_mat = matrix(NA_integer_,
                   ncol = ncol(r) * one_dim,
                   nrow = nrow(r) * one_dim)
}

insert_matrix = function(val_mat, new_mat, starting_x, starting_y){
  nc = ncol(val_mat)
  nr = nrow(val_mat)

  y_dim_start = nr * (starting_y - 1) + 1
  y_dim_end = nr * starting_y

  x_dim_start = nc * (starting_x - 1) + 1
  x_dim_end = nc * starting_x

  new_mat[y_dim_start:y_dim_end, x_dim_start:x_dim_end] = val_mat
  return(new_mat)
}
