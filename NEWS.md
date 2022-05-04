# motif 0.5.2

* Speeds up distance calculations in `lsp_search` and `lsp_compare`
* Further improves support for inputs with the terra's `SpatRaster` class

# motif 0.5.1

* Improves support for inputs with the terra's `SpatRaster` class

# motif 0.5.0

* Adds support for inputs and outputs with the terra's `SpatRaster` class

# motif 0.4.3

* Adds `lsp_restructure()` to split signatures from a list-column into many columns

# motif 0.4.2

* Fixes CRAN gcc-UBSAN/clang-UBSAN RcppArmadillo issue

# motif 0.4.1

* Improves documentation
* Adds small data and small data examples

# motif 0.4.0

* Improves and simplifies tests
* Fixes code to work on cropped stars proxy
* Adds `classes` argument to `lsp_search()`
* Adds a type argument to `lsp_add_quality()`, making it possible to calculate either isolation (type = "segmentation") or distinction (type = "cluster")
* Adds `lsp_add_examples()` to extract and add stars objects to an lsp object
* Adds `lsp_add_examples.sf` to extract and add stars objects to an sf object (the output of `lsp_search()`)
* Adds `lsp_mosaic()` to create artificial landscapes by rearranging spatial data for example regions
* Creates internal `determine_classes()` function
* Creates internal `prepare_window()` function

# motif 0.3.12

* The Nowosad (2021) paper has been published <https://doi.org/10.1007/s10980-020-01135-0>)
* First stable version

# motif 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package
