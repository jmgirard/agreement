# Prepare categorical data for analysis -----------------------------------
prep_data_cat <- function(.data,
                          object,
                          rater,
                          score,
                          categories = NULL,
                          weighting = "identity") {

  out <- list()

  # Ensure df is a tibble
  df <- as_tibble(.data)

  # Select the important variables
  df <- dplyr::select(df, {{object}}, {{rater}}, {{score}})

  # Add explicit NA rows to missing object-rater combinations
  df <- tidyr::complete(df, {{object}}, {{rater}})

  # Reorder df by rater and object so that scores fills out matrix properly
  df <- dplyr::arrange(df, {{rater}}, {{object}})

  # Get and count each variable's unique values
  out$objects <- unique(dplyr::pull(df, {{object}}))
  out$raters <- unique(dplyr::pull(df, {{rater}}))
  out$n_objects <- length(out$objects)
  out$n_raters <- length(out$raters)

  # Pull scores, convert NaN to NA, and count NAs
  scores <- dplyr::pull(df, {{score}})
  scores[is.nan(scores)] <- NA
  out$n_missing_scores <- sum(are_na(scores))

  # Get and count observed categories
  cat_observed <- sort(unique(scores))
  n_cat_observed <- length(cat_observed)

  # If specified, get and count possible categories
  if (is_null(categories)) {
    cat_possible <- cat_observed
    n_cat_possible <- n_cat_observed
  } else {
    if (is.factor(categories)) {
      cat_possible <- levels(categories)
    } else {
      cat_possible <- unique(categories)
    }
    n_cat_possible <- length(cat_possible)
  }
  out$categories <- cat_possible
  out$n_categories <- n_cat_possible

  # Format ratings into an object-by-rater matrix
  out$ratings <- matrix(
    scores,
    nrow = out$n_objects,
    ncol = out$n_raters,
    dimnames = list(out$objects, out$raters)
  )

  # Drop objects and raters that contain only missing values
  out <- remove_uncoded(out)

  # Validate basic counts
  assert_that(out$n_objects >= 1,
              msg = "There must be at least 1 valid object in `.data`.")
  assert_that(out$n_raters >= 2,
              msg = "There must be at least 2 raters in `.data`.")
  assert_that(out$n_categories > 1,
              msg = "There must be at least 2 possible categories.")

  # Validate categories
  cat_unknown <- setdiff(cat_observed, cat_possible)
  assert_that(is_empty(cat_unknown),
              msg = "A category not in `categories` was observed in `.data`.")

  # Get weight matrix
  out$weighting <- weighting
  out$weight_matrix <- calc_weights(weighting, cat_possible)

  out
}

# Drop objects and raters that contain only missing values ----------------
remove_uncoded <- function(x) {
  mat <- x$ratings
  mat <- mat[rowSums(are_na(mat)) != ncol(mat), , drop = FALSE]
  mat <- mat[, colSums(are_na(mat)) != nrow(mat), drop = FALSE]
  out <- x
  out$ratings <- mat
  out$n_objects <- nrow(mat)
  out$n_raters <- ncol(mat)
  out
}

# Calculate chance-adjusted index -----------------------------------------
adjust_chance <- function(poa, pea) {
  (poa - pea) / (1 - pea)
}

# Convert from codes to rater counts in object-by-category matrix ---------
raters_obj_cat <- function(codes, categories) {
  table(
    row(codes),
    factor(unlist(codes), levels = categories),
    useNA = "no"
  )
}

# Convert from codes to object counts in rater-by-category matrix ---------
objects_rat_cat <- function(codes, categories) {
  table(
    col(codes),
    factor(unlist(codes), levels = categories),
    useNA = "no"
  )
}

# Calculate weight matrix -------------------------------------------------
calc_weights <- function(type, categories) {

  # Count the categories
  n_categories <- length(categories)

  # Convert to numeric if necessary
  if (!is.numeric(categories) && type != "identity") {
    category_values <- 1:n_categories
    warning(
      "Numeric categories are required for ", type, " weights.\n",
      "Converting to integers from 1 to the number of categories."
    )
  } else {
    category_values <- categories
  }

  # Start with diagonal matrix
  weight_matrix <- diag(n_categories)
  rownames(weight_matrix) <- categories
  colnames(weight_matrix) <- categories

  # If categories are ordered, calculate weights
  if (type != "identity") {
    max_distance <- diff(range(category_values))
    for (i in seq_along(categories)) {
      for (j in seq_along(categories)) {
        obs_distance <- category_values[[i]] - category_values[[j]]
        if (type == "linear") {
          weight_matrix[i, j] <- 1 - abs(obs_distance) / max_distance
        } else if (type == "quadratic") {
          weight_matrix[i, j] <- 1 - obs_distance^2 / max_distance^2
        }
      }
    }
  }

  weight_matrix
}
