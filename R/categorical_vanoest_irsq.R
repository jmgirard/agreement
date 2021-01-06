#' @export
cat_irsq <- function(.data, ...) {
  cat_adjusted(.data, approach = "irsq", agreement = "pairs", ...)
}

# Worker function to calculate the irsq score and its components
calc_irsq <- function(codes, categories, weight_matrix, agreement) {

  # Calculate percent observed agreement
  poa <- calc_agreement(codes, categories, weight_matrix, agreement)

  # Calculate percent expected agreement
  pea <- calc_chance_irsq(codes, categories, weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Create and label output vector
  out <- c(POA = poa, PEA = pea, CAI = cai)

  out
}

# Worker function to calculate expected agreement using the irsq model of chance
calc_chance_irsq <- function(codes, categories, weight_matrix) {

  # Count important units
  n_objects <- nrow(codes)
  n_raters <- ncol(codes)
  n_categories <- length(categories)

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc)

  # What is the adjusted prevalence of each category?
  exp_c <- (1 + colSums(r_oc)) / (n_categories + sum(r_o))

  # What is the probability of each combination of categories being assigned at random?
  exp_cc <- matrix(exp_c, ncol = 1) %*% matrix(exp_c, nrow = 1)

  # How much chance agreement is expected for each combination of categories?
  pea_cc <- weight_matrix * exp_cc

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(pea_cc)

  pea
}
