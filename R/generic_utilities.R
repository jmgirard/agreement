# Convert wide format to long format
wide_to_long <- function(x, ...) {

  assert_that(is.data.frame(x) || is.matrix(x))

  o <- dplyr::if_else(tibble::has_rownames(x), rownames(x), 1:nrow(x))

  out <-
    x %>%
    dplyr::mutate(Trial = 1, Object = o) %>%
    tidyr::pivot_longer(
      cols = -c(Trial, Object),
      names_to = "Rater",
      values_to = "Score",
      ...
    )

  out
}

# Convert long format to wide format
long_to_wide <- function(x, rater, score, ...) {

  assert_that(is.data.frame(x) || is.matrix(x))

  out <-
    x %>%
    tidyr::pivot_wider(
      names_from = {{rater}},
      values_from = {{score}},
      ...
    )

  out
}

# Convert table format to wide format
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

# Convert table format to long format
table_to_long <- function(x, raters = NULL) {
  wide <- table_to_wide(x, raters)
  long <- wide_to_long(wide)
  long
}
