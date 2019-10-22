#' Calculate Specific Agreement Coefficients
#'
#' Calculate the percent observed agreement for each discrete category in an
#' object-by-rater matrix. Specific agreement is an index of the reliability of
#' categorical measurements. It describes the amount of agreement observed with
#' regard to specific categories. Thus, multiple specific agreement scores are
#' typically used (i.e., one for each category). With two raters, the
#' interpretation of specific agreement for any category is the probability of
#' one rater assigning an item to that category given that the other rater has
#' also assigned that item to that category. With more than two raters, the
#' interpretation becomes the probability of a randomly chosen rater assigning
#' an item to that category given that another randomly chosen rater has also
#' assigned that item to that category. When applied to binary (i.e.,
#' dichotomous) tasks, specific agreement on the positive category is often
#' referred to as positive agreement (PA) and specific agreement on the negative
#' category is often referred to as negative agreement (NA). It is also worth
#' noting that positive agreement (PA) for two raters is equivalent to the F1
#' score commonly used in computer science.
#'
#' @param .data *Required.* An matrix or data frame where rows correspond to
#'   objects of measurement, columns correspond to sources of measurement (e.g.,
#'   raters), and values correspond to category assignments. Note that \code{NA}
#'   will be treated as missing values, so if \code{NA} is a meaningful code in
#'   your data, then recode them to another value and reserve \code{NA} for
#'   missing values.
#' @param categories *Optional.* A numeric vector of possible categories. Useful
#'   when \code{.data} may not contain all possible categories.
#' @param bootstrap *Optional.* A positive integer specifying how many bootstrap
#'   resamples to use in calculating the confidence intervals (default = 2000).
#'   Or, to prevent bootstrapping, set to \code{NULL}.
#' @param interval *Optional.* A real number greater than 0 and less than 1
#'   specifying the width of the confidence interval to calculate such as 0.95
#'   for a 95\% confidence interval (default = 0.95).
#' @param digits *Optional.* A positive integer specifying how many digits to
#'   round the specific agreement estimates to (default = 3). Or, to prevent
#'   rounding, set to \code{NULL}.
#' @param warnings *Optional.* A logical specifying whether to include warnings
#'   (default = TRUE).
#' @return A tibble containing two or four columns and one row for each
#'   category. The first variable is "Category" and contains the name of each
#'   category. The second variable is "Agreement" and contains the estimated
#'   specific agreement coefficient for each category. If \code{bootstrap} is
#'   not NULL, the third variable is "Agreement_LCI" and contains the lower
#'   bound of the bootstrap confidence interval and the the fourth variables is
#'   "Agreement_UCI" and contains the upper bound of the bootstrap confidence
#'   interval.
#' @export
#' @references Uebersax, J. S. (1982). A design-independent method for measuring
#'   the reliability of psychiatric diagnosis. *Journal of Psychiatric Research,
#'   17*(4), 335-342. \url{https://doi.org/10/fbbdfn}
#' @family agreement functions for categorical data
#' @examples
#' data("ordered")
#' cat_specific(ordered, categories = 0:3)
cat_specific <- function(.data,
                          categories = NULL,
                          bootstrap = 2000,
                          interval = 0.95,
                          digits = 3,
                          warnings = TRUE) {

  # Validate inputs
  assert_that(
    is_null(bootstrap) || is.count(bootstrap),
    msg = "The `bootstrap` argument must be either NULL or a positive integer."
  )
  assert_that(is.scalar(interval))
  assert_that(interval > 0 && interval < 1)
  assert_that(
    is_null(digits) || is.count(digits),
    msg = "The `digits` argument must be either NULL or a positive integer."
  )
  assert_that(is.flag(warnings))

  # Prepare data for analysis
  d <- prep_data(.data, categories, "identity", warnings)

  if (is_null(bootstrap)) {
    # Calculate specific agreement without bootstrapping
    sa <- calc_sa(d$codes, d$categories, d$weight_matrix)

    # Construct the output tibble
    out <- tibble(Category = factor(d$categories), Agreement = sa)
  } else {
    # Calculate specific agreement with bootstrapping

    # Warn about samples with less than 20 objects
    if (d$n_objects < 20 && warnings == TRUE) {
      warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
    }

    # Create function to perform bootstrapping
    bs_function <- function(codes, index, categories, weight_matrix) {
      resample <- codes[index, ]
      calc_sa(resample, categories, weight_matrix)
    }

    # Calculate the bootstrap results
    bs_results <-
      boot::boot(
        data = d$codes,
        statistic = bs_function,
        R = bootstrap,
        categories = d$categories,
        weight_matrix = d$weight_matrix
      )

    # Create the output tibble
    out <- tibble(
      Category = d$categories,
      Agreement = bs_results$t0,
      Agreement_LCI = bs_results$t0,
      Agreement_UCI = bs_results$t0
    )

    # Fill in the confidence interval bounds
    for (i in seq_along(d$categories)) {
      out$Agreement_LCI[[i]] <- stats::quantile(
        bs_results$t[, i],
        probs = (1 - interval) / 2,
        na.rm = TRUE
      )
      out$Agreement_UCI[[i]] <- stats::quantile(
        bs_results$t[, i],
        probs = 1 - (1 - interval) / 2,
        na.rm = TRUE
      )
    }
  }

  # Round if requested
  if (!is_null(digits)) {
    out <- dplyr::mutate_if(out, is.numeric, round, digits = digits)
  }

  out
}

# Worker function to calculate specific agreement
calc_sa <- function(codes, categories, weight_matrix) {

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories = categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc, na.rm = TRUE)

  # How much agreement was observed for each object-category combination?
  obs_oc <- r_oc * (t(weight_matrix %*% t(r_oc)) - 1)

  # How much agreement was observed for each category across all objects?
  obs_c <- colSums(obs_oc, na.rm = TRUE)

  # How much agreement was possible for each object-category combination?
  max_oc <- r_oc * (r_o - 1)

  # How much agreement was possible for each category across all objects?
  max_c <- colSums(max_oc, na.rm = TRUE)

  # What was the percent observed agreement for each category across all objects?
  poa_c <- obs_c / max_c

  # Replace non-finite results with missing values
  poa_c[!is.finite(poa_c)] <- NA

  # Label the output vector with category names
  names(poa_c) <- categories

  poa_c
}

