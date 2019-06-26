# Calculate weight matrix
get_weights <- function(type, categories) {
  categories <- get_unique(categories)
  n_categories <- length(categories)
  if (!is.numeric(categories)) {
    categories <- 1:n_categories
  }
  weight_matrix <- diag(n_categories)
  if (type == "identity" || type == 0) {
    return(weight_matrix)
  }
  max_distance <- diff(range(categories))
  for (i in seq_along(categories)) {
    for (j in seq_along(categories)) {
      obs_distance <- categories[[i]] - categories[[j]]
      if (type == "linear" || type == 1) {
        weight_matrix[i, j] <- 1 - abs(obs_distance) / max_distance
      } else if (type == "quadratic" || type == 2) {
        weight_matrix[i, j] <- 1 - obs_distance^2 / max_distance^2
      }
    }
  }
  weight_matrix
}


# Traditional Agreement
agree_raw <- function(codes, categories, weight_matrix) {

  n_objects <- nrow(codes)
  n_categories <- length(categories)

  # How many raters assigned each object to each category?
  r_oc <- matrix(0, nrow = n_objects, ncol = n_categories)
  for (k in seq_along(categories)) {
    r_oc[, k] <- rowSums(codes == categories[[k]], na.rm = TRUE)
  }

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc, na.rm = TRUE)

  # How much agreement was observed for each object and each category?
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

# Krippendorff Agreement
agree_alpha <- function(codes, categories, weight_matrix) {

  n_objects <- nrow(codes)
  n_categories <- length(categories)

  r_oc <- matrix(0, nrow = n_objects, ncol = n_categories)
  for (c in seq_along(categories)) {
    r_oc[, c] <- rowSums(codes == categories[[c]], na.rm = TRUE)
  }
  rstar_oc <- t(weight_matrix %*% t(r_oc))
  r_o <- rowSums(r_oc, na.rm = TRUE)
  r_oc <- r_oc[r_o >= 2, ]
  rstar_oc <- rstar_oc[r_o >= 2, ]
  r_o <- r_o[r_o >= 2]
  rbar <- mean(r_o)
  nprime <- length(r_o)
  epsilon <- 1 / sum(r_o) # should this be 1 / (sum(r_o) * nprime)?
  obs_o <- (r_oc * (rstar_oc - 1)) %*% matrix(1, nrow = n_categories, ncol = 1)
  max_o <- rbar * (r_o - 1)
  poa <- (1 - epsilon) * sum(obs_o / max_o) / nprime + epsilon

  poa
}
