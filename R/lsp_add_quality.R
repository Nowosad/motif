#' @param x
#'
#' @param x_dist
#' @param clust_var
#' @param regions
#'
#' @export
lsp_add_quality = function(x, x_dist, clust_var, regions = FALSE){
  x_dist = as.matrix(x_dist)
  inh = lsp_inhomogeneity(x, x_dist = x_dist, clust_var = clust_var, regions = regions)
  iso = lsp_isolation(x, x_dist = x_dist, clust_var = clust_var, regions = regions)

  x$inhomogeneity = inh[match(x[[clust_var]], seq_along(inh))]
  x$isolation = iso[match(x[[clust_var]], seq_along(iso))]
  x$quality = get_quality(x$inhomogeneity, x$isolation)

  x
}

get_quality = function(inh, iso){
  qua = 1 - (inh / iso)
}

lsp_isolation = function(x, x_dist, clust_var, regions){

  x_merged = st_as_sf(x[clust_var], merge = TRUE, connect8 = TRUE)

  if (!regions){
    x_merged = aggregate(x_merged, list(x_merged[[clust_var]]), function(x) x[1])
  } else {
    stop("This option is not yet implemented.")
  }
  x = st_as_sf(x[c("id", clust_var)], merge = FALSE)

  x_grid_neigh = spdep::poly2nb(x_merged, queen = TRUE)
  unique_clust = na.exclude(unique(x[[clust_var]]))

  vapply(unique_clust, get_isolation,
         x = x, x_dist = x_dist,
         x_grid_neigh = x_grid_neigh,
         clust_var = clust_var,
         FUN.VALUE = 1.0)
}

get_isolation = function(x_clust, x, x_dist, x_grid_neigh, clust_var){
  # print(x_clust)
  x_clust_neigh = x_grid_neigh[[x_clust]]
  if (sum(x_clust_neigh) == 0){
    return(NA)
  } else {
    x_clust_dist = vector(mode = "numeric", length = length(x_clust_neigh))
    for (i in seq_along(x_clust_neigh)){
      # print(i)
      x_clust_dist[i] = mean(x_dist[
        x[["id"]][(x[[clust_var]] == x_clust)],
        x[["id"]][(x[[clust_var]] == x_clust_neigh[i])]
        ])
    }
    return(mean(x_clust_dist))
  }
}

lsp_inhomogeneity = function(x, x_dist, clust_var, regions){

  unique_clust = na.exclude(unique(c(x[[clust_var]])))

  vapply(unique_clust, get_inhomogeneity,
         x = x, x_dist = x_dist,
         clust_var = clust_var,
         FUN.VALUE = 1.0)
}

get_inhomogeneity = function(x, x_dist, x_clust, clust_var){
  x_clust_dist = x_dist[
    x[["id"]][(x[[clust_var]] == x_clust)],
    x[["id"]][(x[[clust_var]] == x_clust)]
    ]

  if (sum(x_clust_dist) != 0){
    x_clust_dist = as.dist(x_clust_dist)
    return(mean(x_clust_dist))
  } else {
    return(0)
  }
}
