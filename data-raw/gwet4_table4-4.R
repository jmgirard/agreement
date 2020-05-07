# page 120
gwet4_table4.4 <-
  tibble::tribble(
    ~Rater1, ~Rater2, ~Rater3, ~Rater4,
    "a", "a", NA, "a",
    "b", "b", "c", "b",
    "c", "c", "c", "c",
    "c", "c", "c", "c",
    "b", "b", "b", "b",
    "a", "b", "c", "d",
    "d", "d", "d", "d",
    "a", "a", "b", "a",
    "b", "b", "b", "b",
    NA, "e", "e", "e",
    NA, NA, "a", "a",
    NA, NA, "c", NA
  )

usethis::use_data(gwet4_table4.4, overwrite = TRUE)
