#include "get_motifel_size.h"
#include "create_attributes.h"
#include <comat.h>
// [[Rcpp::depends(comat)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::interfaces(r, cpp)]]
using namespace Rcpp;







/*** R
library(raster)
set.seed(10100)
l1 = raster(matrix(sample(1:2, size = 1000000, replace = TRUE), ncol = 1000))
l2 = raster(matrix(sample(c(9, 6, 3), size = 1000000, replace = TRUE), ncol = 1000))
x = stack(l1, l2, l1)
rasters = lapply(raster::as.list(x), raster::as.matrix)
directions = as.matrix(4)
a = list(as.matrix(l1))
res = get_motifels(a, directions = directions, size = 100, shift = 100)
library(tibble)
res_df = as_tibble(res)

get_cove(res_df$V3[[4]])
*/
