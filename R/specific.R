#' Calculate specific agreement from an object-by-rater matrix
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
#' @param weighting *Optional.* A string indicating what weighting scheme to use
#'   when calculating agreement. Options are "identity" for unordered categories
#'   and "linear" or "quadratic" for ordered categories (default = "identity").
#' @param bootstrap *Optional.* A positive integer specifying how many bootstrap
#'   resamples to use in calculating the confidence intervals (default = 2000).
#'   Or, to prevent bootstrapping, set to \code{NULL}.
#' @param interval *Optional.* A real number greater than 0 and less than 1
#'   specifying the type of confidence interval to calculate (default = 0.95).
#' @param digits *Optional.* A positive integer specifying how many digits to
#'   round the specific agreement estimates to (default = 3). Or, to prevent
#'   rounding, set to \code{NULL}.
#' @return A tibble containing two or four columns and one row for each
#'   category. The first variable is "Category" and contains the name of each
#'   category. The second variable is "SA_EST" and contains the estimated
#'   specific agreement coefficient for each category. If \code{bootstrap} is
#'   not NULL, the third variable is "SA_LCI" and contains the lower bound of
#'   the bootstrap confidence interval and the the fourth variables is "SA_UCI"
#'   and contains the upper bound of the bootstrap confidence interval.
#' @export
#' @references Uebersax, J. S. (1982). A design-independent method for measuring
#'   the reliability of psychiatric diagnosis. *Journal of Psychiatric Research,
#'   17*(4), 335-342. \url{https://doi.org/10/fbbdfn}
#' @examples
#' data("fish")
#' calc_specific(fish, categories = 1:3)
calc_specific <- function(.data,
                          categories = NULL,
                          weighting = c("identity", "linear", "quadratic"),
                          bootstrap = 2000,
                          interval = 0.95,
                          digits = 3) {

  # Validate inputs
  weighting <- match.arg(weighting)
  assertthat::assert_that(assertthat::is.count(digits),
    msg = "The `digits` argument must be either NULL or a positive integer.")
  assertthat::assert_that(assertthat::is.scalar(interval))
  assertthat::assert_that(interval > 0 && interval < 1)
  assertthat::assert_that(is.null(bootstrap) || assertthat::is.count(bootstrap),
    msg = "The `bootstrap` argument must be either NULL or a positive integer.")

  # Prepare data for analysis
  d <- prep_data(.data, categories, weighting)

  if (is.null(bootstrap)) {
    # Calculate specific agreement without bootstrapping
    sa <- calc_sa(d$codes, d$categories, d$weight_matrix)

    # Construct the output tibble
    out <- tibble::tibble(Category = d$categories, SA_EST = sa)

    # Round if requested
    if (!is.null(digits)) {
      out <- dplyr::mutate(out, SA_EST = round(SA_EST, digits = digits))
    }
  } else {
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
    out <- tibble::tibble(
      Category = d$categories,
      SA_EST = bs_results$t0,
      SA_LCI = bs_results$t0,
      SA_UCI = bs_results$t0
    )

    # Fill in the confidence interval bounds
    for (i in seq_along(d$categories)) {
      bsi <- boot::boot.ci(bs_results, conf = interval, type = "perc", index = i)
      out$SA_LCI[[i]] <- bsi$percent[[4]]
      out$SA_UCI[[i]] <- bsi$percent[[5]]
    }

    # Round if requested
    if (!is.null(digits)) {
      out <- dplyr::mutate_at(
        out,
        .vars = vars(SA_EST:SA_UCI),
        .funs = round,
        digits = digits
      )
    }
  }

  out
}

# Worker function to calculate specific agreement
calc_sa <- function(codes, categories, weight_matrix) {

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories = categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc, na.rm = TRUE)

  # How much agreement was observed for each object and each category?
  obs_oc <- r_oc * (t(weight_matrix %*% t(r_oc)) - 1)

  # How much agreement was observed for each category across all objects?
  obs_c <- colSums(obs_oc, na.rm = TRUE)

  # How much agreement was maximally possible for each object and each category?
  max_oc <- r_oc * (r_o - 1)

  # How much agreement was maximally possible for each category?
  max_c <- colSums(max_oc, na.rm = TRUE)

  # What was the percent observed agreement for each category?
  poa_c <- obs_c / max_c

  # Replace indefinables with missing values
  poa_c[is.nan(poa_c)] <- NA

  # Label the output vector with category names
  names(poa_c) <- categories

  poa_c
}

