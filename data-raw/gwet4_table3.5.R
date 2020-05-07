# Gwet (2014) page 80
gwet4_table3.5 <-
  tibble::tribble(
    ~Units, ~Rater1, ~Rater2,
    1, "A", NA,
    2, "B", "C",
    3, "C", "C",
    4, "C", "C",
    5, "B", "B",
    6, "B", NA,
    7, "A", "A",
    8, "A", "B",
    9, "B", "B",
    10, "B", "B",
    11, NA, "C"
  ) %>%
  dplyr::mutate_at(
    dplyr::vars(Rater1, Rater2),
    ~factor(., levels = LETTERS[1:3], ordered = TRUE)
  )
usethis::use_data(gwet4_table3.5, overwrite = TRUE)

gwet4_table3.5_long <-
  gwet4_table3.5 %>%
  tidyr::pivot_longer(
    cols = Rater1:Rater2,
    names_to = "Rater",
    values_to = "Score"
  )
usethis::use_data(gwet4_table3.5_long, overwrite = TRUE)
