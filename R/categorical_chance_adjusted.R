#' Calculate Chance-Adjusted Agreement
#'
#' Description
#'
#' @param .data *Required.* A matrix or data frame in tall format containing
#'   categorical data where each row corresponds to a single score (i.e.,
#'   assignment of an object to a category) Cells should contain numbers or
#'   characters indicating the discrete category that the corresponding rater
#'   assigned the corresponding object to. Cells should contain \code{NA} if a
#'   particular assignment is missing (e.g., that object was not assigned to a
#'   category by that rater).
#' @param object *Optional.* The name of the variable in \code{.data}
#'   identifying the object of measurement for each observation, in non-standard
#'   evaluation without quotation marks. (default = \code{Object})
#' @param rater *Optional.* The name of the variable in \code{.data} identifying
#'   the rater or source of measurement for each observation, in non-standard
#'   evaluation without quotation marks. (default = \code{Rater})
#' @param score *Optional.* The name of the variable in \code{.data} containing
#'   the categorical score or rating/code for each observation, in non-standard
#'   evaluation without quotation marks. (default = \code{Score})
#' @param approach *Optional.* A string or vector of strings specifying the
#'   chance-adjustment approach(es) to use. Currently, the "alpha", "gamma",
#'   "irsq", "kappa", "pi", and "s" approaches are available. (default =
#'   c("alpha", "gamma", "kappa", "irsq", "pi", "s"))
#' @param categories *Optional.* A vector (numeric, character, or factor)
#'   containing all possible categories that objects could have been assigned
#'   to. When this argument is omitted or set to \code{NULL}, the possible
#'   categories are assumed to be those observed in \code{.data}. However, in
#'   the event that not all possible categories are observed in \code{.data},
#'   this assumption may be misleading and so the possible categories, and their
#'   ordering, can be explicitly specified. (default = NULL)
#' @param weighting *Optional.* A single string specifying the type of weighting
#'   scheme to use. Weighting schemes allow the accommodation of ordered and
#'   unordered categories with the same formulas. Currently, "identity" weights
#'   are available for unordered/nominal categories and both "linear" and
#'   "quadratic" weights are available for ordered categories. (default =
#'   "identity")
#' @param agreement *Optional.* Either \code{NULL} or a single string specifying
#'   the formula to use in calculating percent observed agreement. Currently,
#'   "objects" is available to calculate agreement averaged across objects,
#'   "pairs" is available to calculate agreement averaged across object-rater
#'   pairs, and "kripp" is available to calculate agreement using Krippendorff's
#'   formula. \code{NULL} sets agreement to the default formula for each
#'   approach (i.e., "kripp" for Krippendorff's alpha, "pairs" for Van Oest's
#'   irsq, and "objects" for all others). (default = NULL)
#' @param bootstrap *Optional.* A single non-negative integer that specifies how
#'   many bootstrap resamplings should be computed (used primarily for
#'   estimating confidence intervals and visualizing uncertainty). To skip
#'   bootstrapping, set this argument to 0. (default = 2000)
#' @param warnings *Optional.* A single logical value that specifies whether
#'   warnings should be displayed. (default = TRUE).
#' @return An object of type 'cai' containing the results and details.
#'   \describe{\item{approach}{A character vector containing the name of each
#'   approach in order} \item{observed}{A numeric vector containing the raw
#'   observed agreement according to each approach} \item{expected}{A numeric
#'   vector containing the expected chance agreement according to each approach}
#'   \item{adjusted}{A numeric vector containing the chance-adjusted agreement
#'   according to each approach. Note that these values are those typically
#'   named after each approach (e.g., this is the kappa coefficient)}
#'   \item{boot_results}{A list containing the results of the bootstrap
#'   procedure} \item{details}{A list containing the details of the analysis,
#'   such as the formatted \code{codes}, relevant counts, weighting scheme and
#'   weight matrix.} \item{call}{The function call that created these results.}}
#' @references Gwet, K. L. (2014). *Handbook of inter-rater reliability: The
#'   definitive guide to measuring the extent of agreement among raters* (4th
#'   ed.). Gaithersburg, MD: Advanced Analytics.
#' @references van Oest, R. (2019). A new coefficient of interrater agreement:
#'   The challenge of highly unequal category proportions. *Psychological
#'   Methods, 24*(4), 439-451. \url{https://doi.org/10/ggbk3f}
#' @family functions for categorical data
#' @family functions for chance-adjusted agreement
#' @export
cat_adjusted <- function(.data,
                         object = Object,
                         rater = Rater,
                         score = Score,
                         approach = c("alpha", "gamma", "irsq", "kappa", "pi", "s"),
                         categories = NULL,
                         weighting = c("identity", "linear", "quadratic"),
                         agreement = NULL,
                         bootstrap = 2000,
                         warnings = TRUE) {

  # Validate inputs
  assert_that(is.data.frame(.data) || is.matrix(.data))
  approach <- match.arg(approach, several.ok = TRUE)
  approach <- unique(approach)
  assert_that(is_null(categories) || is_vector(categories))
  weighting <- match.arg(weighting)
  assert_that(is.null(agreement) || agreement %in% c("objects", "pairs", "kripp"))
  assert_that(bootstrap == 0 || is.count(bootstrap))
  assert_that(is.flag(warnings))

  # Prepare .data for analysis
  d <- prep_data_cat(
    .data = .data,
    object = {{object}},
    rater = {{rater}},
    score = {{score}},
    approach = approach,
    categories = categories,
    weighting = weighting,
    agreement = agreement,
    bootstrap = bootstrap
  )

  # Prepare empty results in case of errors
  n_approach <- length(approach)
  out <- new_cai(
    approach = approach,
    observed = rep(NA_real_, n_approach),
    expected = rep(NA_real_, n_approach),
    adjusted = rep(NA_real_, n_approach),
    boot_results = list(
      t = matrix(NA, nrow = 1, ncol = 3 * n_approach),
      t0 = rep(NA, 3 * n_approach)
    ),
    details = d,
    call = match.call()
  )

  # Warn about bootstrapping samples with less than 20 objects
  if (d$n_objects < 20 && bootstrap > 0 && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

  # Warn about bootstrapping with fewer than 1000 resamples
  if (bootstrap > 0 && bootstrap < 1000 && warnings == TRUE) {
    warning("To get stable confidence intervals, consider using more bootstrap resamples.")
  }

  # Warn about there being fewer than 2 categories
  if (d$n_categories < 2) {
    if (warnings == TRUE) {
      warning("Only a single category was observed or requested. Returning NA.\nHint: Try setting the possible categories explicitly with the categories argument")
    }
    return(out)
  }
  if (d$n_raters < 2) {
    if (warnings == TRUE) {
      warning("Only a single rater was observed. Returning NA.")
    }
    return(out)
  }

  # Create function to perform bootstrapping
  boot_function <- function(ratings,
                            index,
                            function_list,
                            categories,
                            weight_matrix,
                            agreement) {

    resample <- ratings[index, , drop = FALSE]
    bsr <- rep(NA_real_, times = length(function_list) * 3)
    # Loop through approaches
    for (i in seq_along(function_list)) {
      bsr[(i * 3 - 2):(i * 3)] <- function_list[[i]](
        codes = resample,
        categories = categories,
        weight_matrix = weight_matrix,
        agreement = agreement
      )
    }

    bsr
  }

  # Collect functions into vector to speed up bootstrapping
  expr_list <- parse(text = paste0("calc_", approach))
  function_list <- NULL
  for (i in 1:length(expr_list)) {
    function_list <- c(function_list, eval(expr_list[[i]]))
  }

  # Calculate the bootstrap results
  boot_results <-
    boot::boot(
      data = d$ratings,
      statistic = boot_function,
      R = bootstrap,
      function_list = function_list,
      categories = d$categories,
      weight_matrix = d$weight_matrix,
      agreement = d$agreement
    )

  # Construct cai class output object
  out <- new_cai(
    approach = approach,
    observed = boot_results$t0[seq(from = 1, to = length(approach) * 3, by = 3)],
    expected = boot_results$t0[seq(from = 2, to = length(approach) * 3, by = 3)],
    adjusted = boot_results$t0[seq(from = 3, to = length(approach) * 3, by = 3)],
    boot_results = boot_results,
    details = d,
    call = match.call()
  )

  out
}
