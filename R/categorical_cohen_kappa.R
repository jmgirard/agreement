#' @export
cat_kappa <- function(.data,
                      categories = NULL,
                      weighting = c("identity", "linear", "quadratic"),
                      bootstrap = 2000,
                      interval = 0.95,
                      digits = 3,
                      details = FALSE,
                      warnings = TRUE) {

  cat_cai(.data, "kappa", categories, weighting,
          bootstrap, interval, digits, details, warnings)

}

# Calculate the kappa coefficient and its components
calc_kappa <- function(codes, categories, weight_matrix) {

  # Calculate percent observed agreement
  poa <- calc_agreement(codes, categories, weight_matrix)

  # Calculate percent expected agreement
  pea <- calc_chance_kappa(codes, categories, weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Create and label output vector
  out <- c(POA = poa, PEA = pea, CAI = cai)

  out
}

# Calculate expected agreement using the kappa model of chance
calc_chance_kappa <- function(codes, categories, weight_matrix) {

  n_raters <- ncol(codes)
  n_categories <- length(categories)

  # How many objects did each rater assign to each category?
  o_rc <- objects_rat_cat(codes, categories)

  # How many objects did each rater assign to any category?
  o_r <- rowSums(o_rc)

  # How many objects could each rater have assigned to each category?
  omax_rc <- o_r %*% matrix(1, ncol = n_categories)

  # What was the prevalence of each category for each rater?
  prev_rc <- o_rc / omax_rc

  # What was the prevalance of each category, averaged across all raters?
  prev_c <- colMeans(prev_rc)

  # TODO: Add interpretations and informative variable names
  x <- t(prev_rc) %*% prev_rc #dot-products for each rater-category combination
  y <- prev_c %*% t(prev_c) #dot-products for the average rater
  z <- (x - n_raters * y) / (n_raters - 1) #scaling or correction?

  # What is the probability of two categories being assigned at random?
  exp_cc <- y - z / n_raters

  # How much chance agreement is expected for each combination of categories?
  pea_cc <- weight_matrix * exp_cc

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(pea_cc, na.rm = TRUE)

  pea
}
