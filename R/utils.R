# Prepare data for analysis
prep_data <- function(.data, categories = NULL, weighting = "identity") {

  # Extract codes from .data
  codes <- as.matrix(.data)

  # Drop objects that were never coded
  codes <- remove_uncoded(codes)

  # Get basic counts
  n_objects <- nrow(codes)
  n_raters <- ncol(codes)

  # Validate basic counts
  assertthat::assert_that(n_objects >= 1)
  assertthat::assert_that(n_raters >= 2)

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
  assertthat::assert_that(rlang::is_empty(cat_unknown))

  # Get weight matrix
  weight_matrix <- get_weights(weighting, cat_possible)

  out <- list(
    codes = codes,
    cat_possible = cat_possible,
    weight_matrix = weight_matrix
  )

  out
}

finite <- function(x) {
  x[is.finite(x)]
}

get_unique <- function(x) {
  x %>%
    c() %>%
    unique() %>%
    finite() %>%
    sort()
}

# Drop rows that contain only missing values
remove_uncoded <- function(mat) {
  mat[rowSums(is.na(mat)) != ncol(mat), ]
}

# Calculate chance-adjusted index
adjust_chance <- function(poa, pea) {
  (poa - pea) / (1 - pea)
}
