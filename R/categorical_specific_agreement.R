#' Calculate Category-Specific Agreement
#'
#' Specific agreement is an index of the reliability of categorical
#' measurements. It describes the amount of agreement observed with regard to
#' each possible category. With two raters, the interpretation of specific
#' agreement for any category is the probability of one rater assigning an item
#' to that category given that the other rater has also assigned that item to
#' that category. With more than two raters, the interpretation becomes the
#' probability of a randomly chosen rater assigning an item to that category
#' given that another randomly chosen rater has also assigned that item to that
#' category. When applied to binary (i.e., dichotomous) data, specific agreement
#' on the positive category is often referred to as positive agreement (PA) and
#' specific agreement on the negative category is often referred to as negative
#' agreement (NA). In this case, PA is equal to the F1 score frequently used in
#' computer science.
#'
#' @inheritParams cat_adjusted
#' @return An object of type 'spa' containing the results and details.
#'   \describe{\item{observed}{A named numeric vector containing the observed
#'   specific agreement for each category} \item{boot_results}{A list containing
#'   the results of the bootstrapping procedure} \item{details}{A list
#'   containing the details of the analysis, such as the formatted \code{codes},
#'   relevant counts, weighting scheme and weight matrix.} \item{call}{The
#'   function call that created these results.}}
#' @references Dice, L. R. (1945). Measures of the amount of ecologic
#'   association between species. *Ecology, 26*(3), 297-302.
#'   \url{https://doi.org/10/dsb8pd}
#' @references Fleiss, J. L. (1975). Measuring agreement between two judges on the presence or absence of a trait. *Biometrics, 31*(3), 651-659. \url{https://doi.org/10/fxdb8x}
#' @references Uebersax, J. S. (1982). A design-independent method for measuring
#'   the reliability of psychiatric diagnosis. *Journal of Psychiatric Research,
#'   17*(4), 335-342. \url{https://doi.org/10/fbbdfn}
#' @references Cicchetti, D. V., & Feinstein, A. R. (1990). High agreement but
#'   low kappa: II. Resolving the paradoxes. *Journal of Clinical Epidemiology,
#'   43*(6), 551-558. \url{https://doi.org/10/czkxkb}
#' @family functions for categorical data
#' @family functions for specific agreement
#' @export
cat_specific <- function(.data,
                         object = Object,
                         rater = Rater,
                         score = Score,
                         categories = NULL,
                         bootstrap = 2000,
                         warnings = TRUE) {

  # Validate inputs
  assert_that(is.data.frame(.data) || is.matrix(.data))
  assert_that(is_null(categories) || is_vector(categories))
  assert_that(bootstrap == 0 || is.count(bootstrap))
  assert_that(is.flag(warnings))

  # Prepare data for analysis
  d <- prep_data_cat(.data, {{object}}, {{rater}}, {{score}}, categories)

  # Warn about samples with less than 20 objects
  if (bootstrap > 0 && d$n_objects < 20 && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

  # Warn about bootstrapping with fewer than 1000 resamples
  if (bootstrap > 0 && bootstrap < 1000 && warnings == TRUE) {
    warning("To get stable confidence intervals, consider using more bootstrap resamples.")
  }

  # Create function to perform bootstrapping
  boot_function <- function(ratings, index, categories, weight_matrix) {
    resample <- ratings[index, , drop = FALSE]
    calc_sa(resample, categories, weight_matrix)
  }

  # Calculate the bootstrap results
  boot_results <-
    boot::boot(
      data = d$ratings,
      statistic = boot_function,
      R = bootstrap,
      categories = d$categories,
      weight_matrix = d$weight_matrix
    )

  # Construct spa class output object
  out <- new_spa(
    observed = boot_results$t0,
    boot_results = boot_results,
    details = d,
    call = match.call()
  )

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

