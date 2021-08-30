
<!-- README.md is generated from README.Rmd. Please edit the .Rmd file. -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ReboundTools)](https://cran.r-project.org/package=ECCTools)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/earamendia/ECCTools/workflows/R-CMD-check/badge.svg)](https://github.com/earamendia/ECCTools/actions)
[![codecov](https://codecov.io/gh/earamendia/ECCTools/branch/develop/graph/badge.svg?token=IG68VLDYEY)](https://codecov.io/gh/earamendia/ECCTools)
[![Codecov test
coverage](https://codecov.io/gh/earamendia/ECCTools/branch/master/graph/badge.svg)](https://codecov.io/gh/earamendia/ECCTools?branch=master)
<!-- badges: end -->

# ECCTools

The `R` package `ECCTools` provides tools to modify and Energy
Conversion Chain (ECC) that has been previously loaded using the `R`
package `IEATools`. The `IEATools` package helps to load IEA data as
well as to sort and prepare IEA data to formulate a Physical Supply Use
Table (see @Heun:2018). However, the `IEATools` package treats imports
as a supplying industry, and to this extent, adopts what we refer here
to as the Free Imports Assumption: imports come for free, with no
upstream associated conversion chain (and hence, no environmental burden
and embodied energy).

The `ECCTools` package enables analysts to overcome this limitation
through two different options:

1.  The formulation of a Multi-Regional Physical Supply Use Table
    (MR-PSUT) framework representing the Energy Conversion Chain. In
    this case, imports are treated as supply coming from an industry
    located in a different region. This option is the most realistic
    one.
2.  The formulation of a PSUT framework following the Domestic
    Technology Assumption (DTA). The DTA assumes that imports are
    produced within the region that imports them. Although the DTA is
    not a realistic assumption, it enables analysts to conduct useful
    simulations.

## Installation

You can install `ECCTools` from github with:

``` r
# install devtools if not already installed
# install.packages("devtools")
devtools::install_github("earamendia/ECCTools")
# To build vignettes locally, use
devtools::install_github("earamendia/ECCTools", build_vignettes = TRUE)
```

## History

This package builds upon the previous `R` package `IEATools`, and has
been first demonstrated in the paper @mr\_psut\_paper .

## More Information

Find more information, including vignettes and function documentation,
at <https://earamendia.github.io/ECCTools/>.
