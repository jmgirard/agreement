#' Convert table format to wide format
#'
#' Description
#'
#' @param x *Required.* A matrix, dataframe, or table object representing a
#'   two-by-two contingency matrix (AKA confusion matrix).
#' @param raters *Optional.* A character vector containing the name of each
#'   rater.
#' @return A tibble containing the data from \code{x} in wide format.
#' @export
table_to_wide <- function(x, raters = NULL) {
  requireNamespace("DescTools", quietly = TRUE)
  wide <- DescTools::Untable(x)
  if (is_null(raters)) {
    colnames(wide) <- paste0("R", 1:ncol(wide))
  } else {
    colnames(wide) <- raters
  }
  wide <- tibble::as_tibble(wide)

  wide
}

#' Convert table format to long format
#'
#' Description
#'
#' @inheritParams table_to_wide
#' @return A tibble containing the data from \code{x} in long format.
#' @export
table_to_long <- function(x, raters = NULL) {
  wide <- table_to_wide(x, raters)
  long <- wide_to_long(wide)
  long
}

#' @export
wide_to_long <- function(x) {
  requireNamespace("tidyr", quietly = TRUE)
  wide <- dplyr::mutate(x, Object = dplyr::row_number(), .before = 1)
  long <- tidyr::pivot_longer(
    wide,
    cols = -Object,
    names_to = "Rater",
    values_to = "Score"
  )
  long
}
