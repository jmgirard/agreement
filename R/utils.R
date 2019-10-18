# Prepare data for analysis
prep_data <- function(.data, categories, weighting) {

  # Extract codes from .data
  codes <- as.matrix(.data)

  # Drop objects that were never coded
  codes <- remove_uncoded(codes)

  # Get basic counts
  n_objects <- nrow(codes)
  n_raters <- ncol(codes)

  # Validate basic counts
  assertthat::assert_that(n_objects >= 1,
    msg = "There must be at least 1 valid object in `.data`.")
  assertthat::assert_that(n_raters >= 2,
    msg = "There must be at least 2 raters in `.data`.")

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
  assertthat::assert_that(rlang::is_empty(cat_unknown),
    msg = "A category not in `categories` was observed in `.data`.")

  # Get weight matrix
  weight_matrix <- get_weights(weighting, cat_possible)

  out <- list(
    codes = codes,
    n_objects = n_objects,
    n_raters = n_raters,
    n_categories = n_cat_possible,
    categories = cat_possible,
    weight_matrix = weight_matrix
  )

  out
}

# Drop any non-finite values
finite <- function(x) {
  x[is.finite(x)]
}

# Return sorted unique finite values
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

# Convert from codes to rater counts in object-by-category matrix
raters_obj_cat <- function(codes, categories = NULL) {
  table(
    row(codes),
    factor(unlist(codes), levels = categories),
    useNA = "no"
  )
}

