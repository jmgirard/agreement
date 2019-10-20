#' @export
cat_s <- function(.data,
                   categories = NULL,
                   weighting = c("identity", "linear", "quadratic"),
                   bootstrap = 2000,
                   interval = 0.95,
                   digits = 3,
                   details = FALSE,
                   warnings = TRUE) {

  cat_cai(.data, "s", categories, weighting,
          bootstrap, interval, digits, details, warnings)

}

# Worker function to calculate the S score and its components
calc_s <- function(codes, categories, weight_matrix) {

  # Calculate percent observed agreement
  poa <- calc_agreement(codes, categories, weight_matrix)

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
