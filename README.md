
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ananke

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/ananke/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/ananke/actions)
[![codecov](https://codecov.io/gh/tesselle/ananke/branch/main/graph/badge.svg?token=UgoOXsZW86)](https://app.codecov.io/gh/tesselle/ananke)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/ananke/badge/main)](https://www.codefactor.io/repository/github/tesselle/ananke/overview/main)
[![Dependencies](https://tinyverse.netlify.com/badge/ananke)](https://cran.r-project.org/package=ananke)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/ananke"
alt="r-universe" /></a>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

**ananke** provides functions for radiocarbon calibration and
chronological analysis.

## Installation

You can install the released version of **ananke** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ananke")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/ananke")
```

## Usage

``` r
## Load packages
library(ananke)
```

``` r
## Data from Bosch et al. 2015
data("ksarakil")

## Calibrate multiple dates
cal <- c14_calibrate(
  ages = ksarakil$date,
  errors = ksarakil$error,
  names = ksarakil$code,
  curves = "marine13",
  reservoir_offsets = 53,
  reservoir_errors = 43,
  from = 50000, to = 0
)

## Plot
plot(cal)
```

![](man/figures/README-calibration-1.png)<!-- -->

``` r
plot(cal, density = FALSE, level = 0.95, lwd = 5)
```

![](man/figures/README-calibration-2.png)<!-- -->

## Contributing

Please note that the **ananke** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.
