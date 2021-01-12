#' @export
cat_s <- function(.data, ...) {
  cat_adjusted(.data, approach = "s", agreement = "objects", ...)
}

# Worker function to calculate the S score and its components
calc_s <- function(codes, categories, weight_matrix, agreement, ...) {

  # Default to agreement averaged over objects
  if (is.null(agreement)) agreement <- "objects"

  # Calculate percent observed agreement
  poa <- calc_agreement(codes, categories, weight_matrix, agreement)

  # Calculate percent expected agreement
  pea <- calc_chance_s(codes, categories, weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Create and label output vector
  out <- c(POA = poa, PEA = pea, CAI = cai)

  out
}

# Worker function to calculate expected agreement using the S model of chance
calc_chance_s <- function(codes, categories, weight_matrix) {

  # How many categories were possible?
  n_categories <- length(categories)

  # How much chance agreement is expected for each combination of categories?
  pea_cc <- weight_matrix / n_categories^2

  # How much chance agreement is expected across all combinations of categories?
  pea <- sum(pea_cc)

  pea
}
