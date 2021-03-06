#' @export
cat_alpha <- function(.data, ...) {
  cat_adjusted(.data, approach = "alpha", agreement = "kripp", ...)
}

# Calculate the alpha coefficient and its components
calc_alpha <- function(codes, categories, weight_matrix, agreement, ...) {

  # Default to agreement using Krippendorff's formula
  if (is.null(agreement)) agreement <- "kripp"

  # Calculate percent observed agreement
  poa <- calc_agreement(codes, categories, weight_matrix, agreement)

  # Calculate percent expected agreement
  pea <- calc_chance_alpha(codes, categories, weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Create and label output vector
  out <- c(POA = poa, PEA = pea, CAI = cai)

  out
}

# Calculate expected agreement using the alpha model of chance
calc_chance_alpha <- function(codes, categories, weight_matrix) {

  n_objects <- nrow(codes)
  n_categories <- length(categories)

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc)

  # TODO: Add interpretation and informative name
  rstar_oc <- t(weight_matrix %*% t(r_oc))

  # Remove objects with fewer than 2 raters (i.e., singletons)
  r_oc <- r_oc[r_o >= 2, ]
  r_o <- r_o[r_o >= 2]

  # How many objects were rated by 2 or more raters?
  n_objects_many <- length(r_o)

  # What is the average number of raters per object (after removing singletons)?
  n_raters_avg <- mean(r_o)

  # TODO: Add interpretation and informative name
  pi_c <- t(t(rep(1 / n_objects_many, n_objects_many)) %*% (r_oc / n_raters_avg))

  # What percent agreement is expected by chance across all categories?
  pea <- sum(weight_matrix * (pi_c %*% t(pi_c)))

  pea
}
