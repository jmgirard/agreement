#' Convert wide format to long format
#'
#' Description
#'
#' @param x *Required.* A data frame containing rating data in wide format.
#' @param ... *Optional.* Additional arguments passed on to
#'   \code{tidyr::pivot_longer()/}.
#' @return A tibble containing the data from \code{x} but in long format.
#' @export
wide_to_long <- function(x, ...) {

  assert_that(is.data.frame(x) || is.matrix(x))

  o <- dplyr::if_else(tibble::has_rownames(x), rownames(x), 1:nrow(x))

  out <-
    x %>%
    as_tibble() %>%
    dplyr::mutate(Trial = 1, Object = o) %>%
    tidyr::pivot_longer(
      cols = -c(Trial, Object),
      names_to = "Rater",
      values_to = "Score",
      ...
    )

  out
}

#' Convert long format to wide format
#'
#' Description
#'
#' @param x *Required.* A data frame containing rating data in long format.
#' @param rater *Required.* The name of the variable in \code{x} that indicates
#'   the rater for each measurement.
#' @param score *Required.* The name of the variable in \code{x} that indicates
#'   the score for each measurement.
#' @param ... *Optional.* Additional arguments passed on to
#'   \code{tidyr::pivot_wider()}.
#' @return A tibble containing the data from \code{x} but in wide format.
#' @export
long_to_wide <- function(x, rater, score, ...) {

  assert_that(is.data.frame(x) || is.matrix(x))

  out <-
    x %>%
    as_tibble() %>%
    tidyr::pivot_wider(
      names_from = {{rater}},
      values_from = {{score}},
      ...
    )

  out
}

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
  wide <- as_tibble(wide)

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
