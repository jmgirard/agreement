interval_wide <-
  tibble::tribble(
    ~R1, ~R2, ~R3, ~R4,
    1.0, 1.5, 1.0,  NA,
    2.0, 2.0, 2.0, 2.0,
    0.5, 1.0, 1.5, 1.5,
    1.0, 1.0, 1.0, 1.0,
    1.0, 1.0, 1.0, 1.5,
     NA, 1.0, 2.5,  NA,
    2.5, 2.5, 2.5, 2.5,
    1.0, 1.0,  NA, 1.0,
     NA, 1.0, 2.0, 1.0,
    1.0, 1.0, 0.5, 1.0,
    1.5, 1.5, 1.5, 1.5,
    1.0, 1.5, 1.0,  NA,
    1.0, 1.0, 1.5,  NA,
    1.0, 2.0, 2.5, 2.0,
     NA, 1.0, 1.5, 1.0,
    0.5, 0.5, 0.5, 0.5
  )
usethis::use_data(interval_wide, overwrite = TRUE)

interval <-
  interval_wide %>%
  dplyr::mutate(Object = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = R1:R4, names_to = "Rater", values_to = "Score")
usethis::use_data(interval, overwrite = TRUE)
