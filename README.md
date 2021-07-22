
<!-- README.md is generated from README.Rmd. Please edit that file -->

# motif <img src="man/figures/logo.png" align="right" width="150" />

<!-- badges: start -->

[![CRAN
status](http://www.r-pkg.org/badges/version/motif)](https://cran.r-project.org/package=motif)
[![GitHub action build
status](https://github.com/Nowosad/motif/workflows/pkgdown/badge.svg)](https://github.com/Nowosad/motif/actions)
[![Codecov test
coverage](https://codecov.io/gh/Nowosad/motif/branch/master/graph/badge.svg)](https://codecov.io/gh/Nowosad/motif?branch=master)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/motif)](https://cran.r-project.org/package=motif)
<!-- badges: end -->

The **motif** package implements and extends ideas of the pattern-based
spatial analysis in R. It describes spatial patterns of categorical
raster data for any defined regular and irregular areas. Patterns are
represented quantitatively using built-in signatures based on
co-occurrence matrices but also allows for any user-defined functions.
It enables spatial analysis such as search, change detection, and
clustering to be performed on spatial patterns.

## Installation

You can install the released version of motif from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("motif")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Nowosad/motif")
```

## Documentation

See the package’s vignettes:

1.  [Introduction to the motif
    package](https://nowosad.github.io/motif/articles/v1_intro.html)
2.  [Types of spatial patterns’
    signatures](https://nowosad.github.io/motif/articles/articles/v2_signatures.html)
3.  [Spatial patterns’
    search](https://nowosad.github.io/motif/articles/articles/v3_search.html)
4.  [Spatial patterns’
    comparision](https://nowosad.github.io/motif/articles/articles/v4_compare.html)
5.  [Spatial patterns’
    clustering](https://nowosad.github.io/motif/articles/articles/v5_cluster.html)
6.  [Extracting the proportion of different classes in many regular
    local
    landscapes](https://nowosad.github.io/motif/articles/articles/v6_composition.html)

## Contribution

Contributions to this package are welcome. The preferred method of
contribution is through a GitHub pull request. Feel free to contact us
by creating [an issue](https://github.com/Nowosad/motif/issues).

## Citation

To cite the `motif` package in publications, please use [this
paper](https://doi.org/10.1007/s10980-020-01135-0):

Nowosad, J. Motif: an open-source R tool for pattern-based spatial
analysis. Landscape Ecol (2021).
<https://doi.org/10.1007/s10980-020-01135-0>

LaTeX/BibTeX version can be obtained with:

    library(motif)
    citation("motif")
