chance_ckappa <- function(codes, categories, weight_matrix) {

  n_objects <- nrow(codes)
  n_raters <- ncol(codes)
  n_categories <- length(categories)

  # How many objects did each rater assign to each category?
  obs_rc <- matrix(0, nrow = n_raters, ncol = n_categories)
  for (k in seq_along(categories)) {
    obs_rc[, k] <- colSums(codes == categories[[k]], na.rm = TRUE)
  }

  # How many objects did each rater assign to any category?
  obs_r <- rowSums(obs_rc, na.rm = TRUE)

  # How many objects could each rater have assigned to each category?
  max_rc <- obs_r %*% matrix(1, nrow = 1, ncol = n_categories)

  # What was the prevalence of each category for each rater?
  prev_rc <- obs_rc / max_rc

  # What was the prevalance of each category, averaged across all raters?
  prev_c <- colMeans(prev_rc, na.rm = TRUE)

  # TODO: Add interpretation
  x <- t(prev_rc) %*% prev_rc

  # TODO: Add interpretation
  y <- prev_c %*% t(prev_c)

  # TODO: Add interpretation
  z <- (x - n_raters * y) / (n_raters - 1)

  # What is the weighted probability of assigning the same category by chance?
  chance <- sum(weight_matrix * (y - z / n_raters))

  chance
}
