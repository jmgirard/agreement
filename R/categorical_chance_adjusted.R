#' @export
cat_cai <- function(.data,
                    approach = c("s", "gamma", "kappa", "pi", "alpha"),
                    categories = NULL,
                    weighting = c("identity", "linear", "quadratic"),
                    bootstrap = 2000,
                    interval = 0.95,
                    digits = 3,
                    details = FALSE,
                    warnings = TRUE) {

  # Validate inputs
  approach <- match.arg(approach, several.ok = TRUE)
  weighting <- match.arg(weighting)
  assert_that(
    is_null(bootstrap) || is.count(bootstrap),
    msg = "The `bootstrap` argument must be either NULL or a positive integer."
  )
  assert_that(is.scalar(interval))
  assert_that(interval > 0 && interval < 1)
  assert_that(
    is.count(digits),
    msg = "The `digits` argument must be either NULL or a positive integer."
  )
  assert_that(is.flag(details))
  assert_that(is.flag(warnings))

  d <- prep_data(.data, categories, weighting, warnings)

  # Warn about bootstrapping samples with less than 20 objects
  if (d$n_objects < 20 && !is_null(bootstrap) && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

  out <- tibble(
    Approach = approach,
    Weights = weighting,
    Observed = NA,
    Expected = NA,
    Adjusted = NA,
    Adjusted_LCI = NA,
    Adjusted_UCI = NA,
    nObjects = d$n_objects,
    nRaters = d$n_raters,
    nCategories = d$n_categories,
    nResamples = NA,
    CI_Width = interval
  )

  for (i in seq_along(approach)) {
    approach_i <- approach[[i]]
    function_i <- eval(parse(text = paste0("calc_", approach_i)))

    if (is_null(bootstrap)) {
      # Calculate statistic without bootstrapping
      statistic <- function_i(d$codes, d$categories, d$weight_matrix)

      # Add results to output tibble
      out$Observed[[i]] <- statistic[[1]]
      out$Expected[[i]] <- statistic[[2]]
      out$Adjusted[[i]] <- statistic[[3]]

      drop_cols <- 6:12
    } else {
      # Calculate statistic with bootstrapping

      # Create function to perform bootstrapping
      bs_function <- function(codes, index, categories, weight_matrix) {
        resample <- codes[index, ]
        function_i(resample, categories, weight_matrix)
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

      # Add results to output tibble
      out$Observed[[i]] <- bs_results$t0[[1]]
      out$Expected[[i]] <- bs_results$t0[[2]]
      out$Adjusted[[i]] <- bs_results$t0[[3]]
      out$Adjusted_LCI[[i]] <- stats::quantile(
        bs_results$t[, 3],
        probs = (1 - interval) / 2,
        na.rm = TRUE
      )
      out$Adjusted_UCI[[i]] <- stats::quantile(
        bs_results$t[, 3],
        probs = 1 - (1 - interval) / 2,
        na.rm = TRUE
      )
      out$nResamples = sum(!are_na(bs_results$t[, 3]))

      drop_cols <- 8:12
    }
  }

  # Drop detail columns if requested
  if (details == FALSE) {
    out <- dplyr::select(out, -drop_cols)
  }

  # Round numeric columns if requested
  if (!is_null(digits)) {
    out <- dplyr::mutate_if(out, is.numeric, round, digits = digits)
  }

  out
}
