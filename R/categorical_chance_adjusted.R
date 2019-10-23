#' @export
cat_adjusted <- function(.data,
                    approach = c("s", "gamma", "kappa", "pi", "alpha"),
                    categories = NULL,
                    weighting = c("identity", "linear", "quadratic"),
                    bootstrap = 2000,
                    warnings = TRUE) {

  # Validate inputs
  assert_that(is.data.frame(.data) || is.matrix(.data))
  approach <- match.arg(approach, several.ok = TRUE)
  approach <- unique(approach)
  weighting <- match.arg(weighting)
  assert_that(bootstrap == 0 || is.count(bootstrap))
  assert_that(is.flag(warnings))

  # Prepare .data for analysis
  d <- prep_data(.data, categories, weighting, warnings)

  # Warn about bootstrapping samples with less than 20 objects
  if (d$n_objects < 20 && bootstrap > 0 && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

  # Warn about bootstrapping with fewer than 1000 resamples
  if (bootstrap > 0 && bootstrap < 1000 && warnings == TRUE) {
    warning("To get stable confidence intervals, consider using more bootstrap resamples.")
  }

  # Create function to perform bootstrapping
  boot_function <- function(codes, index, function_list, categories, weight_matrix) {
    resample <- codes[index, ]
    bsr <- rep(NA_real_, times = length(function_list) * 3)
    # Loop through approaches
    for (i in seq_along(function_list)) {
      bsr[(i * 3 - 2):(i * 3)] <- function_list[[i]](resample, categories, weight_matrix)
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
      data = d$codes,
      statistic = boot_function,
      R = bootstrap,
      function_list = function_list,
      categories = d$categories,
      weight_matrix = d$weight_matrix
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
