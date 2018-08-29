agree <- function(.data, categories = NULL, weighting = "identity") {

  # Extract codes from .data
  codes <- .data %>% as.matrix()

  # Drop objects that were never coded
  codes <- remove_uncoded(codes)

  # Get basic counts
  n_objects <- nrow(codes)
  n_raters <- ncol(codes)

  # Validate basic counts
  assert_that(n_objects >= 1)
  assert_that(n_raters >= 2)

  # Get and count observed categories
  cat_observed <- get_unique(codes)
  n_cat_observed <- length(cat_observed)

  # If specified, get and count possible categories
  if (is.null(categories)) {
    cat_possible <- cat_observed
    n_cat_possible <- n_cat_observed
  } else {
    cat_possible <- get_unique(categories)
    n_cat_possible <- length(cat_possible)
  }

  # Check observed categories against possible categories
  cat_unknown <- setdiff(cat_observed, cat_possible)
  assert_that(rlang::is_empty(cat_unknown))

  # Get weight matrix
  weight_matrix <- get_weights(cat_possible, weighting)

  # Get percent observed agreement
  poa <- agree_raw(codes, cat_possible, weight_matrix)
  pea_kappa <- chance_ckappa(codes, cat_possible, weight_matrix)

}

agree_raw <- function(codes, categories, weight_matrix) {

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

  # How much weighted "credit" was gained for each
  credit <- t(weight_matrix %*% t(r_oc))

  # Calculate the agreements
  mat_agrees <- mat_raters * (mat_credit - 1)
  a_observed <- rowSums(mat_agrees)
  a_possible <- cnt_raters * (cnt_raters - 1)
  a_percent <- mean(a_observed[cnt_raters >= 2] / a_possible[cnt_raters >= 2])

  a_percent
}

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
