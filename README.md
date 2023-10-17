
<!-- README.md is generated from README.Rmd. Please edit that file -->

# uqimf

<!-- badges: start -->
<!-- badges: end -->

The goal of uqimf is to …

## Contents of the repository

The folders of this repository contain the following: \* `R`: all
functions and that the analysis is based on - `plot.R`: plotting files -
`score.R`: functions to score quantile forecasts (coverage,
interval_score, crps by sample) - `uqfc.R`: functions to generate
quantile forecasts from past errors - `uqimf-package.R` some stuff, as
this is technically an R package - `utils.R` miscellaneous small
functions - `utils-manuscript.R` miscellaneous small functions for
manuscript

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
