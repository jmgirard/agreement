#
prep_data_dim <- function(.data) {

  d <- list()

  d$ratings <- as.matrix(.data)
  d$ratings[is.nan(d$ratings)] <- NA
  d$n_objects <- nrow(d$ratings)
  d$n_raters <- ncol(d$ratings)
  d$n_missing <- sum(!is.finite(d$ratings))
  d$minimum <- min(d$ratings)
  d$maximum <- max(d$ratings)

  # Validate basic counts
  assert_that(d$n_objects >= 1, msg = "There must be at least 1 object in `.data`.")
  assert_that(d$n_raters >= 2, msg = "There must be at least 2 raters in `.data`.")

  d
  #TODO: coerce to array if repetitions are included
}

prep_data_dim_long <- function(df, object, rater, trial, score) {

  df2 <- dplyr::select(df, {{object}}, {{rater}}, {{trial}}, {{score}})
  df2 <- dplyr::arrange(df2, {{object}}, {{rater}}, {{trial}}, {{score}})
  objects <- unique(dplyr::pull(df2, {{object}}))
  raters <- unique(dplyr::pull(df2, {{rater}}))
  trials <- unique(dplyr::pull(df2, {{trial}}))
  n_objects <- length(objects)
  n_raters <- length(raters)
  n_trials <- length(trials)
  scores <- dplyr::pull(df2, {{score}})
  a <- array(
    scores,
    dim = c(n_objects, n_raters, n_trials),
    dimnames = list(objects, raters, trials)
  )

  a
}
