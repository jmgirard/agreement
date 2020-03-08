# Assumes .data is in long format
prep_data_dim <- function(.data, object, rater, score, trial) {

  out <- list()

  df <- as_tibble(.data)
  if (rlang::quo_is_missing(rlang::enquo(trial))) {
    df <- dplyr::mutate(df, Trial = "T1")
    trial <- rlang::quo(Trial)
  }
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
  scores[is.nan(scores)] <- NA
  out$n_missing_scores <- sum(are_na(scores))
  out$min_score <- min(scores, na.rm = TRUE)
  out$max_score <- max(scores, na.rm = TRUE)
  out$ratings <- array(
    scores,
    dim = c(out$n_objects, out$n_raters, out$n_trials),
    dimnames = list(out$objects, out$raters, out$trials)
  )

  # Validate basic counts
  assert_that(out$n_objects >= 1,
              msg = "There must be at least 1 object in `.data`.")
  assert_that(out$n_raters >= 2,
              msg = "There must be at least 2 raters in `.data`.")
  #TODO: Is intra-rater reliability still calculable with 1 rater?

  out
}

# Turn x into 0 if negative
nonneg <- function(x) {
  max(c(0, x))
}
