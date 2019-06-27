
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agreement

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of the `agreement` package is to calculate estimates of
inter-rater reliability using generalized formulas and output results in
a tidy dataframe format that makes collecting, bootstrapping, and
plotting them easy. The package includes functions for all major
chance-adjusted indexes of categorical agreement (i.e., kappa, alpha,
gamma, pi, and S) as well as all major intraclass correlation
coefficients (i.e., one-way and two-way models, agreement and
consistency types, and single measure and average measure units).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgirard/agreement")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(agreement)
# Load example dataset with 4 raters and 16 objects
data(gwet3.3)
print(gwet3.3)
#>      L   K   W   B
#> 1  1.0 1.5 1.0  NA
#> 2  2.0 2.0 2.0 2.0
#> 3  0.5 1.0 1.5 1.5
#> 4  1.0 1.0 1.0 1.0
#> 5  1.0 1.0 1.0 1.5
#> 6   NA 1.0 2.5  NA
#> 7  2.5 2.5 2.5 2.5
#> 8  1.0 1.0  NA 1.0
#> 9   NA 1.0 2.0 1.0
#> 10 1.0 1.0 0.5 1.0
#> 11 1.5 1.5 1.5 1.5
#> 12 1.0 1.5 1.0  NA
#> 13 1.0 1.0 1.5  NA
#> 14 1.0 2.0 2.5 2.0
#> 15  NA 1.0 1.5 1.0
#> 16 0.5 0.5 0.5 0.5
```

``` r
# Calculate kappa assuming unordered categories
calc_kappa(gwet3.3)
#> # A tibble: 1 x 5
#>   Approach Weights  Observed Expected Adjusted
#>   <chr>    <chr>       <dbl>    <dbl>    <dbl>
#> 1 kappa    identity    0.562    0.283    0.389
```

``` r
# Calculate agreement assuming ordered categories
calc_all(gwet3.3, weighting = "quadratic")
#> # A tibble: 5 x 5
#>   Approach Weights   Observed Expected Adjusted
#>   <chr>    <chr>        <dbl>    <dbl>    <dbl>
#> 1 S        quadratic    0.921    0.75     0.682
#> 2 gamma    quadratic    0.921    0.646    0.775
#> 3 kappa    quadratic    0.921    0.831    0.529
#> 4 pi       quadratic    0.921    0.838    0.511
#> 5 alpha    quadratic    0.936    0.834    0.618
```

## Code of Conduct

Please note that the ‘agreement’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
