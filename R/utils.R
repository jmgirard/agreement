# Prepare data for analysis
prep_data <- function(.data, categories, weighting, warnings) {

  # Extract codes from .data
  codes <- as.matrix(.data)

  # Drop objects that were never coded
  codes <- remove_uncoded(codes)

  # Get basic counts
  n_objects <- nrow(codes)
  n_raters <- ncol(codes)

  # Validate basic counts
  assert_that(n_objects >= 1, msg = "There must be at least 1 valid object in `.data`.")
  assert_that(n_raters >= 2, msg = "There must be at least 2 raters in `.data`.")

  # Get and count observed categories
  cat_observed <- get_unique(codes)
  n_cat_observed <- length(cat_observed)

  # If specified, get and count possible categories
  if (is_null(categories)) {
    cat_possible <- cat_observed
    n_cat_possible <- n_cat_observed
  } else {
    cat_possible <- categories
    n_cat_possible <- length(cat_possible)
  }

  # Check observed categories against possible categories
  cat_unknown <- setdiff(cat_observed, cat_possible)
  assert_that(is_empty(cat_unknown), msg = "A category not in `categories` was observed in `.data`.")

  # Get weight matrix
  weight_matrix <- calc_weights(weighting, cat_possible, warnings)

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

# Drop any values that are missing or NaN
usable <- function(x) {
  x[!are_na(x) & !is.nan(x)]
}

# Return sorted unique finite values
get_unique <- function(x) {
  if (is.data.frame(x)) { x <- as.matrix(x) }
  sort(usable(unique(c(x))))
}

# Drop rows that contain only missing values
remove_uncoded <- function(mat) {
  mat[rowSums(are_na(mat)) != ncol(mat), ]
}

# Calculate chance-adjusted index
adjust_chance <- function(poa, pea) {
  (poa - pea) / (1 - pea)
}

# Convert from codes to rater counts in object-by-category matrix
raters_obj_cat <- function(codes, categories) {
  table(
    row(codes),
    factor(unlist(codes), levels = categories),
    useNA = "no"
  )
}

# Convert from codes to object counts in rater-by-category matrix
objects_rat_cat <- function(codes, categories) {
  table(
    col(codes),
    factor(unlist(codes), levels = categories),
    useNA = "no"
  )
}
