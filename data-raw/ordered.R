# Ordered categorical data, numerical, wide -------------------------------
ordered_wide <-
  tibble::tribble(
    ~R1, ~R2, ~R3, ~R4, ~R5,
    1, 1, 2, NA, 2,
    1, 1, 0, 1, NA,
    2, 3, 3, 3, NA,
    NA, 0, 0, NA, 0,
    0, 0, 0, NA, 0,
    0, 0, 0, NA, 0,
    1, 0, 2, NA, 1,
    1, NA, 2, 0, NA,
    2, 2, 2, NA, 2,
    2, 1, 1, 1, NA,
    NA, 1, 0, 0, NA,
    0, 0, 0, 0, NA,
    1, 2, 2, 2, NA,
    3, 3, 2, 2, 3,
    1, 1, 1, NA, 1,
    1, 1, 1, NA, 1,
    2, 1, 2, NA, 2,
    1, 2, 3, 3, NA,
    1, 1, 0, 1, NA,
    0, 0, 0, NA, 0
  )
usethis::use_data(ordered_wide, overwrite = TRUE)

# Ordered categorical data, numerical, tall -------------------------------
ordered <-
  ordered_wide %>%
  dplyr::mutate(Object = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = R1:R5, names_to = "Rater", values_to = "Score")
usethis::use_data(ordered, overwrite = TRUE)
