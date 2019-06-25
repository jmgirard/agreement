
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agreement

<!-- badges: start -->

<!-- badges: end -->

The goal of agreement is to …

## Installation

You can install the released version of agreement from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("agreement")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgirard/agreement")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(agreement)
data(fish)
calc_all(fish, categories = c(1, 2, 3), weighting = "identity")
#> # A tibble: 4 x 5
#>   Approach Weights  Observed Expected Adjusted
#>   <chr>    <chr>       <dbl>    <dbl>    <dbl>
#> 1 S        identity    0.675    0.333    0.513
#> 2 gamma    identity    0.675    0.142    0.621
#> 3 kappa    identity    0.675    0.725   -0.182
#> 4 pi       identity    0.675    0.717   -0.147
```

## Code of Conduct

Please note that the ‘agreement’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
