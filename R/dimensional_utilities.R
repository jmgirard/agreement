# Assumes .data is in long format
prep_data_dim <- function(.data, object, rater, score, trial) {

  d <- list()

  df <- as_tibble(.data)
  if (rlang::quo_is_null(rlang::enquo(trial))) {
    df <- dplyr::mutate(df, Trial = "T1")
    trial <- rlang::quo(Trial)
  }
  df <- dplyr::select(df, {{object}}, {{rater}}, {{trial}}, {{score}})
  df <- tidyr::complete(df, {{object}}, {{rater}}, {{trial}})
  df <- dplyr::arrange(df, {{trial}}, {{rater}}, {{object}})
  d$objects <- unique(dplyr::pull(df, {{object}}))
  d$raters <- unique(dplyr::pull(df, {{rater}}))
  d$trials <- unique(dplyr::pull(df, {{trial}}))
  d$n_objects <- length(d$objects)
  d$n_raters <- length(d$raters)
  d$n_trials <- length(d$trials)
  scores <- dplyr::pull(df, {{score}})
  scores[is.nan(scores)] <- NA
  d$n_missing_scores <- sum(are_na(scores))
  d$min_score <- min(scores, na.rm = TRUE)
  d$max_score <- max(scores, na.rm = TRUE)
  d$ratings <- array(
    scores,
    dim = c(d$n_objects, d$n_raters, d$n_trials),
    dimnames = list(d$objects, d$raters, d$trials)
  )

  d
}

# Turn x into 0 if negative
nonneg <- function(x) {
  max(c(0, x))
}
