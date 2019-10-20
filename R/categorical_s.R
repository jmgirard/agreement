#' Calculate the S Score
#'
#' Bennett et al.'s S score is a chance-adjusted index of categorical agreement.
#' It estimates chance agreement using category-based assumptions. This function
#' calculates this coefficient using a generalized formula that can accommodate
#' multiple raters, multiple categories, and ordered or unordered categories
#' (through a weighting scheme).
#'
#' @param .data *Required.* An matrix or data frame where rows correspond to
#'   objects of measurement, columns correspond to sources of measurement (e.g.,
#'   raters), and values correspond to category assignments. Note that \code{NA}
#'   will be treated as missing values, so if \code{NA} is a meaningful code in
#'   your data, then recode them to another value and reserve \code{NA} for
#'   missing values.
#' @param categories *Optional.* A numeric vector of possible categories. Useful
#'   when \code{.data} may not contain all possible categories.
#' @param weighting *Optional.* A string indicating whether to use "identity"
#'   weights for unordered categories or "linear" or "quadratic" weights for
#'   ordered categories (default = "identity").
#' @param bootstrap *Optional.* A positive integer specifying how many bootstrap
#'   resamples to use in calculating the confidence intervals (default = 2000).
#'   Or, to prevent bootstrapping, set to \code{NULL}.
#' @param interval *Optional.* A real number greater than 0 and less than 1
#'   specifying the width of the confidence interval to calculate such as 0.95
#'   for a 95\% confidence interval (default = 0.95).
#' @param digits *Optional.* A positive integer specifying how many digits to
#'   round the specific agreement estimates to (default = 3). Or, to prevent
#'   rounding, set to \code{NULL}.
#' @param details *Optional.* A logical specifying whether to include additional
#'   columns in the output detailing the number of objects, raters, categories,
#'   valid bootstrap resamples, and interval width (default = \code{FALSE}).
#' @param warnings *Optional.* A logical specifying whether to include warnings
#'   (default = \code{TRUE}).
#'
#' @return A tibble containing the Approach (i.e., S), the weighting scheme
#'   used, the percent agreement observed in the data (unadjusted), the percent
#'   agreement expected based on chance, and the chance-adjusted index (i.e., S
#'   score). If \code{bootstrap} is used, the lower and upper bounds of a
#'   confidence interval for the chance-adjusted index are also provided. If
#'   \code{details} is \code{TRUE}, additional detail columns are provided.
#'
#' @references Bennett, E. M., Alpert, R., & Goldstein, A. C. (1954).
#'   Communication through limited response questioning. *The Public Opinion
#'   Quarterly, 18*(3), 303-308. \url{https://doi.org/10/brfm77}
#' @references Gwet, K. L. (2014). *Handbook of inter-rater reliability: The
#'   definitive guide to measuring the extent of agreement among raters* (4th
#'   ed.). Gaithersburg, MD: Advanced Analytics.
#' @family agreement functions for categorical data
#' @export
#' @examples
#' data(unordered)
#' cat_s(unordered)
#' data(ordered)
#' cat_s(ordered, weighting = "linear")
cat_s <- function(.data,
                   categories = NULL,
                   weighting = c("identity", "linear", "quadratic"),
                   bootstrap = 2000,
                   interval = 0.95,
                   digits = 3,
                   details = FALSE,
                   warnings = TRUE) {

  cat_cai(.data, "s", categories, weighting,
          bootstrap, interval, digits, details, warnings)

}

# Worker function to calculate the S score and its components
calc_s <- function(codes, categories, weight_matrix) {

  # Calculate percent observed agreement
  poa <- calc_agreement(codes, categories, weight_matrix)

  # Calculate percent expected agreement
  pea <- calc_chance_s(codes, categories, weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Create and label output vector
  out <- c(POA = poa, PEA = pea, CAI = cai)

  out
}

# Worker function to calculate expected agreement using the S model of chance
calc_chance_s <- function(codes, categories, weight_matrix) {

  # How many categories were possible?
  n_categories <- length(categories)

  # How much chance agreement is expected for each combination of categories?
  pea_cc <- weight_matrix / n_categories^2

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(pea_cc)

  pea
}
