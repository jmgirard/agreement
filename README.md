
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

Calculate chance-adjusted indexes of categorical agreement for unordered
categories

``` r
library(agreement)
# Load dataset with 4 raters assigning 12 objects to 5 unordered categories
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
# Calculate all chance-adjusted indexes for unordered categories
results1 <- cat_adjusted(unordered)
#> Warning in cat_adjusted(unordered): With a small number of objects,
#> bootstrap confidence intervals may not be stable.
summary(results1, ci = TRUE)
#> 
#> Call:
#> cat_adjusted(.data = unordered)
#> 
#> Objects =    12
#> Raters =     4
#> Categories =     {a, b, c, d, e}
#> Weighting =  identity
#> 
#> Chance-Adjusted Categorical Agreement with Bootstrapped CIs
#> 
#>         Observed   Expected   Adjusted   2.5 %   97.5 %
#> s          0.818      0.200      0.773   0.531        1
#> gamma      0.818      0.190      0.775   0.541        1
#> kappa      0.818      0.233      0.763   0.479        1
#> pi         0.818      0.239      0.761   0.458        1
#> alpha      0.805      0.240      0.743   0.425        1
```

``` r
# Transform results into a tidy data frame
tidy(results1)
#> # A tibble: 15 x 6
#>    approach weighting term     estimate lower upper
#>    <chr>    <chr>     <chr>       <dbl> <dbl> <dbl>
#>  1 s        identity  Observed    0.818 0.625 1    
#>  2 gamma    identity  Observed    0.818 0.625 1    
#>  3 kappa    identity  Observed    0.818 0.625 1    
#>  4 pi       identity  Observed    0.818 0.625 1    
#>  5 alpha    identity  Observed    0.805 0.600 1.000
#>  6 s        identity  Expected    0.2   0.2   0.2  
#>  7 gamma    identity  Expected    0.190 0.144 0.196
#>  8 kappa    identity  Expected    0.233 0.202 0.420
#>  9 pi       identity  Expected    0.239 0.218 0.424
#> 10 alpha    identity  Expected    0.24  0.216 0.435
#> 11 s        identity  Adjusted    0.773 0.531 1    
#> 12 gamma    identity  Adjusted    0.775 0.541 1    
#> 13 kappa    identity  Adjusted    0.763 0.479 1    
#> 14 pi       identity  Adjusted    0.761 0.458 1    
#> 15 alpha    identity  Adjusted    0.743 0.425 1
```

``` r
# Plot the bootstrap resampling distributions with confidence intervals
plot(results1)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Calculate chance-adjusted indexes of categorical agreement for ordered
categories

``` r
# Load dataset with 5 raters assigning 20 objects to 4 ordered categories
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
# Calculate all chance-adjusted indexes for ordered categories (linear weights)
results2 <- cat_adjusted(ordered, weighting = "linear")
summary(results2, ci = TRUE)
#> 
#> Call:
#> cat_adjusted(.data = ordered, weighting = "linear")
#> 
#> Objects =    20
#> Raters =     5
#> Categories =     {0, 1, 2, 3}
#> Weighting =  linear
#> 
#> Chance-Adjusted Categorical Agreement with Bootstrapped CIs
#> 
#>         Observed   Expected   Adjusted   2.5 %   97.5 %
#> s          0.859      0.583      0.663   0.523    0.803
#> gamma      0.859      0.553      0.686   0.558    0.834
#> kappa      0.859      0.636      0.614   0.394    0.762
#> pi         0.859      0.648      0.601   0.375    0.746
#> alpha      0.864      0.643      0.618   0.404    0.753
```

``` r
tidy(results2)
#> # A tibble: 15 x 6
#>    approach weighting term     estimate lower upper
#>    <chr>    <chr>     <chr>       <dbl> <dbl> <dbl>
#>  1 s        linear    Observed    0.859 0.801 0.918
#>  2 gamma    linear    Observed    0.859 0.801 0.918
#>  3 kappa    linear    Observed    0.859 0.801 0.918
#>  4 pi       linear    Observed    0.859 0.801 0.918
#>  5 alpha    linear    Observed    0.864 0.810 0.919
#>  6 s        linear    Expected    0.583 0.583 0.583
#>  7 gamma    linear    Expected    0.553 0.467 0.575
#>  8 kappa    linear    Expected    0.636 0.576 0.732
#>  9 pi       linear    Expected    0.648 0.592 0.741
#> 10 alpha    linear    Expected    0.643 0.587 0.740
#> 11 s        linear    Adjusted    0.663 0.523 0.803
#> 12 gamma    linear    Adjusted    0.686 0.558 0.834
#> 13 kappa    linear    Adjusted    0.614 0.394 0.762
#> 14 pi       linear    Adjusted    0.601 0.375 0.746
#> 15 alpha    linear    Adjusted    0.618 0.404 0.753
```

``` r
plot(results2)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

Calculate category-specific agreement

``` r
# Calculate category-specific agreement
results3 <- cat_specific(ordered)
summary(results3, ci = TRUE)
#> 
#> Call:
#> cat_specific(.data = ordered)
#> 
#> Objects =    20
#> Raters =     5
#> Categories =     {0, 1, 2, 3}
#> 
#> Category-Specific Agreement with Bootstrapped CIs
#> 
#>     Estimate   2.5 %   97.5 %
#> 0      0.812   0.490    0.954
#> 1      0.605   0.333    0.771
#> 2      0.483   0.171    0.711
#> 3      0.519   0.333    0.667
```

``` r
tidy(results3)
#> # A tibble: 4 x 4
#>   category estimate lower upper
#>      <dbl>    <dbl> <dbl> <dbl>
#> 1        0    0.812 0.490 0.954
#> 2        1    0.605 0.333 0.771
#> 3        2    0.483 0.171 0.711
#> 4        3    0.519 0.333 0.667
```

``` r
plot(results3)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="50%" />

## Code of Conduct

Please note that the ‘agreement’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
