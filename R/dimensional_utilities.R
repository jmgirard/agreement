#
prep_data_dim <- function(.data, warnings = TRUE) {

  d <- list()
  d$ratings <- as.matrix(.data)
  d$ratings[is.nan(d$ratings)] <- NA
  d$n_objects <- nrow(d$ratings)
  d$n_raters <- ncol(d$ratings)
  d$n_missing <- sum(!is.finite(d$ratings))
  d$minimum <- min(d$ratings)
  d$maximum <- max(d$ratings)

  d
  #TODO: coerce to array if repetitions are included
}
