mcgraw_wide <-
  tibble::tribble(
    ~M, ~C,
    103, 119,
    82, 65,
    116, 106,
    102, 102,
    99, 105,
    98, 100,
    104, 107,
    62, 85,
    97, 101,
    107, 110
  )
usethis::use_data(mcgraw_wide, overwrite = TRUE)


# -------------------------------------------------------------------------

mcgraw <-
  mcgraw_wide %>%
  dplyr::mutate(Object = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = M:C, names_to = "Rater", values_to = "Score")
usethis::use_data(mcgraw, overwrite = TRUE)
