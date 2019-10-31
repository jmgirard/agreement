#
prep_data_dim <- function(.data) {

  out <- list()

  out$ratings <- as.matrix(.data)
  out$ratings[is.nan(out$ratings)] <- NA
  out$n_objects <- nrow(out$ratings)
  out$n_raters <- ncol(out$ratings)
  out$n_missing <- sum(!is.finite(out$ratings))
  out$minimum <- min(out$ratings)
  out$maximum <- max(out$ratings)

  # Validate basic counts
  assert_that(out$n_objects >= 1, msg = "There must be at least 1 object in `.data`.")
  assert_that(out$n_raters >= 2, msg = "There must be at least 2 raters in `.data`.")

  out
}

prep_data_dim_long <- function(.data, object, rater, trial, score) {

  out <- list()

  df <- as_tibble(.data)
  df <- dplyr::select(df, {{object}}, {{rater}}, {{trial}}, {{score}})
  df <- tidyr::complete(df, {{object}}, {{rater}}, {{trial}})
  df <- dplyr::arrange(df, {{trial}}, {{rater}}, {{object}})
  out$objects <- unique(dplyr::pull(df, {{object}}))
  out$raters <- unique(dplyr::pull(df, {{rater}}))
  out$trials <- unique(dplyr::pull(df, {{trial}}))
  out$n_objects <- length(out$objects)
  out$n_raters <- length(out$raters)
  out$n_trials <- length(out$trials)
  scores <- dplyr::pull(df, {{score}})
  out$ratings <- array(
    scores,
    dim = c(out$n_objects, out$n_raters, out$n_trials),
    dimnames = list(out$objects, out$raters, out$trials)
  )

  # Validate basic counts
  assert_that(out$n_objects >= 1, msg = "There must be at least 1 object in `.data`.")
  assert_that(out$n_raters >= 2, msg = "There must be at least 2 raters in `.data`.")

  out
}

# Turn x into 0 if negative
nonneg <- function(x) {
  max(c(0, x))
}
