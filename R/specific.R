#' Calculate specific agreement from object-by-rater matrix
#'
#' Calculate the percent observed agreement for each category in an
#' object-by-rater matrix.
#'
#' @param .data A numeric object-by-rater matrix where values are categories.
#' @param categories A numeric vector of possible categories. Useful when
#'   \code{.data} may not contain all possible categories.
#' @return A data frame containing the specific agreement coefficient for each
#'   category.
#' @export
#' @examples
#' data("fish")
#' calc_specific(fish, categories = 1:3)
calc_specific <- function(.data, categories = NULL) {

  # Convenience function to select finite values
  finite <- function(x) {
    x[is.finite(x)]
  }

  # Extract codes from .data
  codes <- as.matrix(.data)

  # Drop objects that were never coded
  codes <- codes[rowSums(is.na(codes)) != ncol(codes), ]

  # Get basic counts
  n_objects <- nrow(codes)
  n_raters <- ncol(codes)

  # Validate basic counts
  stopifnot(n_objects >= 1)
  stopifnot(n_raters >= 2)

  # Get and count observed categories
  cat_observed <- sort(finite(unique(c(codes))))
  n_cat_observed <- length(cat_observed)

  # If specified, get and count possible categories
  if (is.null(categories)) {
    cat_possible <- cat_observed
    n_cat_possible <- n_cat_observed
  } else {
    cat_possible <- sort(finite(unique(c(categories))))
    n_cat_possible <- length(cat_possible)
  }

  # Check observed categories against possible categories
  cat_unknown <- setdiff(cat_observed, cat_possible)
  stopifnot(length(cat_unknown) == 0)

  # Get identity weight matrix
  weight_matrix <- diag(n_cat_possible)

  # How many raters assigned each object to each category?
  r_oc <- matrix(0, nrow = n_objects, ncol = n_cat_possible)
  for (k in seq_along(cat_possible)) {
    r_oc[, k] <- rowSums(codes == cat_possible[[k]], na.rm = TRUE)
  }

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc, na.rm = TRUE)

  # How much agreement was observed for each object and each category?
  obs_oc <- r_oc * (t(weight_matrix %*% t(r_oc)) - 1)

  # How much agreement was observed for each category across all objects?
  obs_c <- colSums(obs_oc, na.rm = TRUE)

  # How much agreement was maximally possible for each object and each category?
  max_oc <- r_oc * (r_o - 1)

  # How much agreement was maximally possible for each category?
  max_c <- colSums(max_oc, na.rm = TRUE)

  # What was the percent observed agreement for each category?
  poa_c <- obs_c / max_c

  # Construct output data frame
  out <- data.frame(Category = cat_possible, SA = poa_c)

  out
}
