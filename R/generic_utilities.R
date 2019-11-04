#
to_long <- function(wide_data, ...) {

  assert_that(is.data.frame(wide_data) || is.matrix(wide_data))

  if (tibble::has_rownames(wide_data)) {
    o <- rownames(wide_data)
  } else {
    o <- 1:nrow(wide_data)
  }

  out <-
    wide_data %>%
    dplyr::mutate(Trial = 1, Object = o) %>%
    tidyr::pivot_longer(
      cols = -c(Trial, Object),
      names_to = "Rater",
      values_to = "Score",
      ...
    )

  out
}
