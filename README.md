
<!-- README.md is generated from README.Rmd. Please edit that file -->

# motif

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/Nowosad/motif.svg?branch=master)](https://travis-ci.org/Nowosad/motif)
[![GitHub action build
status](https://github.com/Nowosad/motif/workflows/pkgdown/badge.svg)](https://github.com/Nowosad/motif/actions)
[![Codecov test
coverage](https://codecov.io/gh/Nowosad/motif/branch/master/graph/badge.svg)](https://codecov.io/gh/Nowosad/motif?branch=master)
<!-- badges: end -->

The **motif** package implements ideas of the pattern-based spatial
analysis in R. It describes spatial patterns of categorical raster data
for any defined regular and irregular areas. Patterns are represented
quantitatively using built-in signatures based on co-occurrence matrices
but also allows for any user-defined functions. It enables spatial
analysis such as search, change detection, and clustering to be
performed on spatial
patterns.

## Installation

<!-- You can install the released version of motif from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("motif") -->

<!-- ``` -->

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Nowosad/motif")
```

## Example

### Signatures

``` r
library(motif)
library(stars)
#> Loading required package: abind
#> Loading required package: sf
#> Linking to GEOS 3.7.1, GDAL 2.3.2, PROJ 5.2.0
landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))
plot(landcover)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
landcover_coma = lsp_thumbprint(landcover, type = "coma", window_size = 200)
landcover_coma
#> # A tibble: 232 x 3
#>       id na_prop signature        
#>  * <int>   <dbl> <list>           
#>  1     3  0.0992 <int[,7] [7 × 7]>
#>  2     4  0.145  <int[,7] [7 × 7]>
#>  3    38  0.255  <int[,7] [7 × 7]>
#>  4    39  0      <int[,7] [7 × 7]>
#>  5    40  0      <int[,7] [7 × 7]>
#>  6    41  0      <int[,7] [7 × 7]>
#>  7    42  0      <int[,7] [7 × 7]>
#>  8    43  0.115  <int[,7] [7 × 7]>
#>  9    77  0      <int[,7] [7 × 7]>
#> 10    78  0      <int[,7] [7 × 7]>
#> # … with 222 more rows
```

``` r
landcover_coma$signature[[1]]
#>    1      2 3 5 6 7    9
#> 1 32     47 0 2 0 0    6
#> 2 47 141250 0 7 0 0  226
#> 3  0      0 0 0 0 0    0
#> 5  2      7 0 4 0 0    0
#> 6  0      0 0 0 0 0    0
#> 7  0      0 0 0 0 0    0
#> 9  6    226 0 0 0 0 1462
```

<!-- ref to vig -->

<!-- ### Search -->

<!-- ### Compare -->

<!-- ### Cluster -->

## Contribution

Contributions to this package are welcome. The preferred method of
contribution is through a GitHub pull request. Feel free to contact us
by creating [an issue](https://github.com/Nowosad/motif/issues).
