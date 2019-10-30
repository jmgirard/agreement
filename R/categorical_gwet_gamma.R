#' @export
cat_gamma <- function(.data, ...) {
  cat_adjusted(.data, approach = "gamma", ...)
}

# Calculate the gamma coefficient and its components
calc_gamma <- function(codes, categories, weight_matrix) {

  # Calculate percent observed agreement
  poa <- calc_agreement(codes, categories, weight_matrix)

  # Calculate percent expected agreement
  pea <- calc_chance_gamma(codes, categories, weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Create and label output vector
  out <- c(POA = poa, PEA = pea, CAI = cai)

  out
}

# Calculate expected agreement using the gamma model of chance
calc_chance_gamma <- function(codes, categories, weight_matrix) {

  n_objects <- nrow(codes)
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
