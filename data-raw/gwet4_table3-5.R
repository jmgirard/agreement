# Gwet (2014) page 80
gwet4_table3.5 <-
  tibble::tribble(
    ~Rater1, ~Rater2,
    "A", NA,
    "B", "C",
    "C", "C",
    "C", "C",
    "B", "B",
    "B", NA,
    "A", "A",
    "A", "B",
    "B", "B",
    "B", "B",
    NA, "C"
  ) %>%
  dplyr::mutate_all(~factor(., levels = LETTERS[1:3], ordered = TRUE))
usethis::use_data(gwet4_table3.5, overwrite = TRUE)

gwet4_table3.5_long <-
  gwet4_table3.5 %>%
  tidyr::pivot_longer(
    cols = Rater1:Rater2,
    names_to = "Rater",
    values_to = "Score"
  )
usethis::use_data(gwet4_table3.5_long, overwrite = TRUE)
