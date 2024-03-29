% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsp_add_examples.R
\name{lsp_add_examples}
\alias{lsp_add_examples}
\alias{lsp_add_examples.lsp}
\alias{lsp_add_examples.sf}
\title{Adds spatial data of each region in an lsp or sf object}
\usage{
lsp_add_examples(x, y, window = NULL)

\method{lsp_add_examples}{lsp}(x, y, window = NULL)

\method{lsp_add_examples}{sf}(x, y, window = NULL)
}
\arguments{
\item{x}{Object of class \code{lsp} - usually a subset of the output of \code{lsp_signature()}
or an object of class \code{sf} - usually a subset of the output of \code{lsp_search()}}

\item{y}{Object of class \code{stars}, \code{stars_proxy}, or terra's \code{SpatRaster}.}

\item{window}{Specifies areas for analysis. It can be either: \code{NULL} or an \code{sf} object.
The \code{sf} object is only needed for adding examples of irregular regions.}
}
\value{
The input object with a new column \code{"region"}.
The \code{"region"} column is a list with a raster extracted for each row.
}
\description{
Adds spatial data of each region in an lsp or sf object.
The output is an lsp or sf object with an additional column \code{"region"}.
See examples.
}
\examples{

library(stars)

landcover = read_stars(system.file("raster/landcover2015s.tif", package = "motif"))

landcover_coma = lsp_signature(landcover, type = "coma", threshold = 0.9, window = 100)
selected_coma = subset(landcover_coma, id \%in\% c(5, 10, 15, 35))
selected_coma

selected_coma = lsp_add_examples(x = selected_coma, y = landcover)
selected_coma

plot(selected_coma$region[[1]])
plot(selected_coma$region[[4]])

\donttest{
# larger data example
library(stars)

landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))

landcover_coma = lsp_signature(landcover, type = "coma", threshold = 0.9, window = 100)
selected_coma = subset(landcover_coma, id \%in\% c(5, 80, 1971, 2048))
selected_coma

selected_coma = lsp_add_examples(x = selected_coma, y = landcover)
selected_coma

plot(selected_coma$region[[1]])
plot(selected_coma$region[[4]])
}
}
