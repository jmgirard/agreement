#' @export
cat_pi <- function(.data, ...) {
  cat_adjusted(.data, approach = "pi", ...)
}

# Calculate the pi coefficient and its components
calc_pi <- function(codes, categories, weight_matrix) {

  # Calculate percent observed agreement
  poa <- calc_agreement(codes, categories, weight_matrix)

  # Calculate percent expected agreement
  pea <- calc_chance_pi(codes, categories, weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Create and label output vector
  out <- c(POA = poa, PEA = pea, CAI = cai)

  out
}

# Calculate expected agreement using the pi model of chance
calc_chance_pi <- function(codes, categories, weight_matrix) {

  n_objects <- nrow(codes)
  n_categories <- length(categories)

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc)

  # How many raters could have assigned each object to each category?
  rmax_oc <- r_o %*% matrix(1, ncol = n_categories)

  # What percent of raters who could have assigned each object to each category did?
  rpct_oc <- r_oc / rmax_oc

  # What is the average prevalence for each category across raters?
  prev_c <- matrix(1 / n_objects, ncol = n_objects) %*% rpct_oc

  # What is the probability of each combination of categories being assigned at random?
  exp_cc <- t(prev_c) %*% prev_c

  # How much chance agreement is expected for each combination of categories?
  pea_cc <- weight_matrix * exp_cc

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(pea_cc)

  pea
}
