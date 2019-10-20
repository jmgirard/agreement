
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
chance-adjusted indexes of categorical agreement (i.e., S, gamma, kappa,
pi, alpha) as well as all major intraclass correlation coefficients
(i.e., one-way and two-way models, agreement and consistency types, and
single measure and average measure units).

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
# Load example dataset with 4 raters assigning 12 objects to 5 unordered categories (and missing values)
data(unordered)
print(unordered)
#> # A tibble: 12 x 4
#>    R1    R2    R3    R4   
#>    <chr> <chr> <chr> <chr>
#>  1 a     a     <NA>  a    
#>  2 b     b     c     b    
#>  3 c     c     c     c    
#>  4 c     c     c     c    
#>  5 b     b     b     b    
#>  6 a     b     c     d    
#>  7 d     d     d     d    
#>  8 a     a     b     a    
#>  9 b     b     b     b    
#> 10 <NA>  e     e     e    
#> 11 <NA>  <NA>  a     a    
#> 12 <NA>  <NA>  c     <NA>
```

``` r
# Calculate chance-adjusted agreement indexes using identity weights
cat_cai(
  .data = unordered, 
  approach = c("s", "gamma", "kappa", "pi", "alpha"),
  categories = c("a", "b", "c", "d", "e"), 
  weighting = "identity",
  bootstrap = NULL
)
#> # A tibble: 5 x 5
#>   Approach Weights  Observed Expected Adjusted
#>   <chr>    <chr>       <dbl>    <dbl>    <dbl>
#> 1 s        identity    0.818    0.2      0.773
#> 2 gamma    identity    0.818    0.19     0.775
#> 3 kappa    identity    0.818    0.233    0.763
#> 4 pi       identity    0.818    0.239    0.761
#> 5 alpha    identity    0.805    0.24     0.743
```

``` r
# Load example dataset with 5 raters assigning 20 objects to 4 ordered categories (and missing values)
data(ordered)
print(ordered)
#> # A tibble: 20 x 5
#>       R1    R2    R3    R4    R5
#>    <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1     1     2    NA     2
#>  2     1     1     0     1    NA
#>  3     2     3     3     3    NA
#>  4    NA     0     0    NA     0
#>  5     0     0     0    NA     0
#>  6     0     0     0    NA     0
#>  7     1     0     2    NA     1
#>  8     1    NA     2     0    NA
#>  9     2     2     2    NA     2
#> 10     2     1     1     1    NA
#> 11    NA     1     0     0    NA
#> 12     0     0     0     0    NA
#> 13     1     2     2     2    NA
#> 14     3     3     2     2     3
#> 15     1     1     1    NA     1
#> 16     1     1     1    NA     1
#> 17     2     1     2    NA     2
#> 18     1     2     3     3    NA
#> 19     1     1     0     1    NA
#> 20     0     0     0    NA     0
```

``` r
# Calculate chance-adjusted agreement indexes using linear weights and bootstrapped 95% confidence intervals
cat_cai(
  .data = ordered, 
  approach = c("s", "gamma", "kappa", "pi", "alpha"),
  weighting = "linear",
  bootstrap = 2000,
  interval = 0.95
)
#> # A tibble: 5 x 7
#>   Approach Weights Observed Expected Adjusted Adjusted_LCI Adjusted_UCI
#>   <chr>    <chr>      <dbl>    <dbl>    <dbl>        <dbl>        <dbl>
#> 1 s        linear     0.859    0.583    0.663        0.516        0.803
#> 2 gamma    linear     0.859    0.553    0.686        0.55         0.834
#> 3 kappa    linear     0.859    0.636    0.614        0.404        0.759
#> 4 pi       linear     0.859    0.648    0.601        0.385        0.746
#> 5 alpha    linear     0.864    0.643    0.618        0.416        0.759
```

## Code of Conduct

Please note that the ‘agreement’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
