
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ooklaOpenDataR

<!-- badges: start -->

<!-- badges: end -->

The goal of ooklaOpenDataR is to make it easier to access data from
Ookla’s [open data
program](https://registry.opendata.aws/speedtest-global-performance/).
This dataset provides global fixed broadband and mobile (cellular)
network performance metrics in zoom level 16 web mercator tiles
(approximately 610.8 meters by 610.8 meters at the equator).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("teamookla/ooklaOpenDataR")
```

## Examples

This is how you can get the global dataset for mobile network data in Q2
2020:

``` r
library(ooklaOpenDataR)

mobile_q2 <- get_performance_tiles(service = "mobile", quarter = 2, year = 2020)
```

Or you can get the fixed broadband data for the same quarter as an `sf`
data frame with the `sf`
argument.

``` r
fixed_q2_sf <- get_performance_tiles(service = "mobile", quarter = 2, year = 2020)
```

The package vignettes demonstrate how to filter the tiles to a
particular area of interest using the `filter_by_quadkey()` function.
