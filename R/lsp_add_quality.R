#' Calculates quality of clustering
#'
#' Calculates three quality metrics to evaluate spatial patterns' clustering:
#' (1) inhomogeneity - it measures a degree of mutual dissimilarity
#' between all objects in a cluster. This value is between 0 and 1,
#' where small value indicates that all objects in the cluster
#' represent consistent patterns so the cluster is pattern-homogeneous.
#' (2) isolation - it is an average distance between the focus cluster
#' and all of its neighbors. This value is between 0 and 1,
#' where large value indicates that the cluster
#' stands out from its surroundings.
#' (3) quality - overall quality of a cluster. It is calculated as
#' 1 - (inhomogeneity / isolation). This value is also between 0 and 1,
#' where increased values indicate increased quality.
#'
#' @param x Object of class `sf` - usually the output of the `lsp_add_clusters()` function
#' @param x_dist Object of class `dist` - usually the output of
#' the `lsp_to_dist()` function
#' @param regions Not implemented yet
#'
#' @seealso lsp_add_clusters
#'
#' @examples
#' # see examples of `lsp_add_clusters()`
#'
#' @export
lsp_add_quality = function(x, x_dist, regions = FALSE){
  if (!inherits(x, "sf")){
    stop("This function requires an sf object as the x argument.", call. = FALSE)
  }
  if (!requireNamespace("spdep", quietly = TRUE)){
    stop("The spdep package is required by this function, please install it first")
  }
  clust_var = "clust"
  x_dist = as.matrix(x_dist)
  inh = lsp_inhomogeneity(x, x_dist = x_dist, clust_var = clust_var, regions = regions)
  iso = lsp_isolation(x, x_dist = x_dist, clust_var = clust_var, regions = regions)

  x$inhomogeneity = inh[match(x[[clust_var]], as.integer(names(inh)))]
  x$isolation = iso[match(x[[clust_var]], as.integer(names(iso)))]
  x$quality = get_quality(x$inhomogeneity, x$isolation)

  x
}

get_quality = function(inh, iso){
  qua = 1 - (inh / iso)
}

lsp_isolation = function(x, x_dist, clust_var, regions){

  x_merged = sf::st_as_sf(x[clust_var], merge = TRUE, connect8 = TRUE)

  if (!regions){
    x_merged = stats::aggregate(x_merged, list(x_merged[[clust_var]]), function(x) x[1])
  } else {
    stop("This option is not yet implemented.", .call = FALSE)
  }
  # for stars I need a table with id and clust...
  x = sf::st_as_sf(x[c("id", clust_var)], merge = FALSE)

  x_grid_neigh = spdep::poly2nb(x_merged, queen = TRUE)
  unique_clust = stats::na.exclude(unique(x[[clust_var]]))

  iso = vapply(unique_clust, get_isolation,
         x = x, x_dist = x_dist,
         x_grid_neigh = x_grid_neigh,
         clust_var = clust_var,
         FUN.VALUE = 1.0)

  names(iso) = unique_clust
  return(iso)
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
        as.character(x[["id"]][(x[[clust_var]] %in% x_clust)]),
        as.character(x[["id"]][(x[[clust_var]] %in% x_clust_neigh[i])])
        ])
    }
    return(mean(x_clust_dist))
  }
}

lsp_inhomogeneity = function(x, x_dist, clust_var, regions){

  unique_clust = stats::na.exclude(unique(c(x[[clust_var]])))

  inh = vapply(unique_clust, get_inhomogeneity,
         x = x, x_dist = x_dist,
         clust_var = clust_var,
         FUN.VALUE = 1.0)
  names(inh) = unique_clust
  return(inh)
}

get_inhomogeneity = function(x, x_dist, x_clust, clust_var){
  x_clust_dist = x_dist[
    as.character(x[["id"]][(x[[clust_var]] %in% x_clust)]),
    as.character(x[["id"]][(x[[clust_var]] %in% x_clust)])
    ]

  if (sum(x_clust_dist) != 0){
    x_clust_dist = stats::as.dist(x_clust_dist)
    return(mean(x_clust_dist))
  } else {
    return(0)
  }
}
