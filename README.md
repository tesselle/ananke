
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ananke

<!-- badges: start -->

[![Code
coverage](https://packages.tesselle.org/ananke/coverage/badge.svg)](https://packages.tesselle.org/ananke/coverage/)
<a href="https://tesselle.r-universe.dev/ananke"
class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/ananke"
alt="r-universe" /></a>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

[![DOI
Zenodo](https://zenodo.org/badge/DOI/10.5281/zenodo.13236285.svg)](https://doi.org/10.5281/zenodo.13236285)
<!-- badges: end -->

## Overview

**ananke** provides functions for simple radiocarbon calibration and
chronological analysis.

This package is currently *experimental*. This means that it is
functional, but interfaces and functionalities may change over time,
testing and documentation may be lacking.

------------------------------------------------------------------------

To cite ananke in publications use:

Frerebeau N (2024). *ananke: Quantitative Chronology in Archaeology*.
Université Bordeaux Montaigne, Pessac, France.
<doi:10.5281/zenodo.13236285> <https://doi.org/10.5281/zenodo.13236285>,
R package version 0.0.1.9000, <https://packages.tesselle.org/ananke/>.

This package is a part of the tesselle project
<https://www.tesselle.org>.

## Installation

You can install the released version of **ananke** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ananke")
```

And the development version from [Codeberg](https://codeberg.org/) with:

``` r
# install.packages("remotes")
remotes::install_git("https://codeberg.org/tesselle/ananke")
```

## Usage

``` r
## Load packages
library(ananke)
#> Loading required package: aion
```

**ananke** uses [**aion**](https://packages.tesselle.org/aion/) for
internal date representation. Look at
`vignette("aion", package = "aion")` before you start.

``` r
## Data from Bosch et al. 2015
data("ksarakil")

## Calibrate multiple ages
cal <- c14_calibrate(
  values = ksarakil$date,
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

## Related Works

- [**Bchron**](https://github.com/andrewcparnell/Bchron) enables quick
  calibration of radiocarbon dates, age-depth modelling, relative sea
  level rate estimation, and non-parametric phase modelling.
- [**rcarbon**](https://github.com/ahb108/rcarbon) includes functions
  not only for basic calibration, uncalibration, and plotting of one or
  more dates, but also a statistical framework for building demographic
  and related longitudinal inferences from aggregate radiocarbon date
  lists.
- [**rintcal**](https://github.com/Maarten14C/rintcal) consists of a
  data compilation of the IntCal radiocarbon calibration curves and
  provides a number of functions to assist with calibrating dates and
  plotting calibration curves.

## Contributing

Please note that the **ananke** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.
