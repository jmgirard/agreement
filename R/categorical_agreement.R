# Calculate percent observed agreement using traditional formula
calc_agreement <- function(codes, categories, weight_matrix) {

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc)

  # How much agreement was observed for each object-category combination?
  obs_oc <- r_oc * (t(weight_matrix %*% t(r_oc)) - 1)

  # How much agreement was observed for each object across all categories?
  obs_o <- rowSums(obs_oc)

  # How much agreement was maximally possible for each object?
  max_o <- r_o * (r_o - 1)

  # What was the percent observed agreement for each object?
  poa_o <- obs_o[r_o >= 2] / max_o[r_o >= 2]

  # What was the percent observed agreement across all objects?
  poa <- mean(poa_o, na.rm = TRUE)

  poa
}

# Calculate percent observed agreement using Krippendorff's formula
calc_agreement_kripp <- function(codes, categories, weight_matrix) {

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc)

  # Remove objects with only 1 rater
  r_oc <- r_oc[r_o >= 2, ]
  r_o <- r_o[r_o >= 2]

  # How much agreement was observed for each object-category combination?
  obs_oc <- r_oc * t(weight_matrix %*% t(r_oc) - 1)

  # What was the average number of raters per object?
  rbar <- mean(r_o)

  # How many objects were rated by 2 or more raters?
  nprime <- length(r_o)

  # How much agreement was observed for each object across all categories?
  obs_o <- rowSums(obs_oc)

  # How much agreement was maximally possible for each object?
  max_o <- rbar * (r_o - 1)

  # What is the standard percent observed agreement?
  poa_prime <- mean(obs_o / max_o, na.rm = TRUE)

  # Correction factor
  epsilon <- 1 / (nprime * rbar)

  # What is the "corrected" percent observed agreement?
  poa <- (1 - epsilon) * poa_prime + epsilon

  poa
}
