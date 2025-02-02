
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The {ti} R package<img src="man/figures/logo.png" align="right" width="25%"/><br><small><font color="#999">Single-channel UINT8 TIFF toolkit</font></small>

<!-- badges: start -->

[![GitHub R package
version](https://img.shields.io/github/r-package/v/rogiersbart/ti?label=version)](https://github.com/rogiersbart/ti)
[![CRAN
status](https://www.r-pkg.org/badges/version/rui)](https://CRAN.R-project.org/package=ti)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of {ti} is to speed up common tasks for managing a TIFF
collection, by providing 1) I/O using deflate compression by default
(here we just wrap {[ijtiff](https://docs.ropensci.org/ijtiff/)}), 2)
conversion to other common 2D/3D image/array formats, 3) tools to go
from a collection of 2D images to a single multi-frame TIFF and vice
versa, and 4) simple common image processing tasks.

## Install

You can install the latest version of {ti} with the following:

``` r
if (!require(pak)) install.packages("pak")
pak::pak("rogiersbart/ti")
```

## Use

All {ti} package functions have the `ff_` prefix, so that reading a TIFF
file is done through:

``` r
img <- ti::ff_read("input-file.tif")
```

and writing can be done with:

``` r
img |> ti::ff_write("output-file.tif")
```

All other functions operate on (an) input file(s), and directly write
the results to (an) output file(s), like:

``` r
ti::ff_compress("input-file.tif", "input-file_compressed.tif") 
ti::ff_tif2bob("input-file.tif", "brick-of-bytes.bob") 
ti::ff_extract("input-file.tif", frame = 13, out = "frame.%frame.tif") 
```

Sensible output file path defaults are in place, which allows you to do:

``` r
ti::ff_compress("input-file.tif") # overwrite with a deflate compressed file
ti::ff_tif2bob("input-file.tif") # will change the extension to .bob
ti::ff_extract("input-file.tif", frame = 13) # will e.g. use extension .013.tif
```

## Note
