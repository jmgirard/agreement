agree_raw <- function(.data, categories = NULL, weighting = NULL) {

  codes <- .data %>% as.matrix()

  # Drop rows with no finite values
  codes <- codes[rowSums(is.na(codes)) != ncol(codes), ]

  # Get basic counts
  n_obj <- nrow(codes)
  n_raters <- ncol(codes)

  # Check basic counts
  if (n_obj < 1) {
    stop("Data contained no valid objects.")
  }

  if (n_raters < 2) {
    stop("Data contained less than two raters.")
  }

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
  if (!rlang::is_empty(cat_unknown)) {
    stop("Data contained unknown categories:\n", cat_unknown)
  }

  # Get weight matrix
  if (is.null(weighting)) {
    mat_w <- diag(n_cat_possible)
  } else {
    mat_w <- get_weights(cat_possible, weighting)
  }

  # Create raters (i.e., object-by-category) matrix
  mat_raters <- matrix(0, nrow = n_obj, ncol = n_cat_possible)
  for (k in seq_along(cat_possible)) {
    codes_k <- codes == cat_possible[[k]]
    mat_raters[, k] <- rowSums(codes_k, na.rm = TRUE)
  }

  # Create "credit" matrix by weighting raters matrix
  mat_credit <- t(mat_w %*% t(mat_raters))

  # Count the number of raters for each object
  cnt_raters <- rowSums(mat_raters, na.rm = TRUE)

  # Calculate the agreements
  mat_agrees <- mat_raters * (mat_credit - 1)
  a_observed <- rowSums(mat_agrees)
  a_possible <- cnt_raters * (cnt_raters - 1)
  a_percent <- mean(a_observed[cnt_raters >= 2] / a_possible[cnt_raters >= 2])

  a_percent
}

weighting <- function(type, categories) {
  categories <- get_unique(categories)
  n_categories <- length(categories)
  if (!is.numeric(categories)) {
    categories <- 1:n_categories
  }
  mat_w <- diag(n_categories)
  if (type == "identity" || type == 0) {
    return(mat_w)
  }
  max_distance <- diff(range(categories))
  for (i in seq_along(categories)) {
    for (j in seq_along(categories)) {
      obs_distance <- categories[[i]] - categories[[j]]
      if (type == "linear" || type == 1) {
        mat_w[i, j] <- 1 - abs(obs_distance) / max_distance
      } else if (type == "quadratic" || type == 2) {
        mat_w[i, j] <- 1 - obs_distance^2 / max_distance^2
      }
    }
  }
  mat_w
}
