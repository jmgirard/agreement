# Cohen's Kappa
chance_kappa <- function(codes, categories, weight_matrix) {

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

  # TODO: Add interpretations and informative variable names
  x <- t(prev_rc) %*% prev_rc
  y <- prev_c %*% t(prev_c)
  z <- (x - n_raters * y) / (n_raters - 1)

  # What is the probability of two categories being assigned at random?
  exp_cc <- y - z / n_raters

  # How much chance agreement is expected for each combination of categories?
  pea_cc <- weight_matrix * exp_cc

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(pea_cc, na.rm = TRUE)

  pea
}

# Scott's Pi
chance_pi <- function(codes, categories, weight_matrix) {

  n_objects <- nrow(codes)
  n_raters <- ncol(codes)
  n_categories <- length(categories)

  # How many raters assigned each object to each category?
  r_oc <- matrix(0, nrow = n_objects, ncol = n_categories)
  for (k in seq_along(categories)) {
    r_oc[, k] <- rowSums(codes == categories[[k]], na.rm = TRUE)
  }

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc, na.rm = TRUE)

  # How many raters could have assigned each object to each category?
  r_oc_max <- r_o %*% matrix(1, nrow = 1, ncol = n_categories)

  # What percent of raters who could have assigned each object to each category did?
  r_oc_pct <- r_oc / r_oc_max

  # What is the average prevalence for each category across raters?
  prev_c <- matrix(1 / n_objects, nrow = 1, ncol = n_objects) %*% r_oc_pct

  # What is the probability of two categories being assigned at random?
  exp_cc <- t(prev_c) %*% prev_c

  # How much chance agreement is expected for each combination of categories?
  pea_cc <- weight_matrix * exp_cc

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(pea_cc, na.rm = TRUE)

  pea
}

# Bennett et al.'s S Score
chance_s <- function(codes, categories, weight_matrix) {

  n_categories <- length(categories)

  # How much chance agreement is expected for each combination of categories?
  pea_cc <- weight_matrix / n_categories^2

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(pea_cc)

  pea
}

# Gwet's Gamma
chance_gamma <- function(codes, categories, weight_matrix) {

  n_objects <- nrow(codes)
  n_raters <- ncol(codes)
  n_categories <- length(categories)

  # How many raters assigned each object to each category?
  r_oc <- matrix(0, nrow = n_objects, ncol = n_categories)
  for (k in seq_along(categories)) {
    r_oc[, k] <- rowSums(codes == categories[[k]], na.rm = TRUE)
  }

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc, na.rm = TRUE)

  # How many raters could have assigned each object to each category?
  r_oc_max <- r_o %*% matrix(1, nrow = 1, ncol = n_categories)

  # What percent of raters who could have assigned each object to each category did?
  r_oc_pct <- r_oc / r_oc_max

  # What is the average prevalence for each category across raters?
  prev_c <- matrix(1 / n_objects, nrow = 1, ncol = n_objects) %*% r_oc_pct

  # TODO: Add interpretations and informative variable names
  a <- prev_c * (1 - prev_c)
  b <- n_categories * (n_categories - 1)

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(weight_matrix) * sum(a) / b

  pea
}
