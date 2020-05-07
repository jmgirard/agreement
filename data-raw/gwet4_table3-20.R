# page 96
gwet4_table3.20 <-
  tibble::tribble(
    ~Rater1, ~Rater2, ~Rater3,
    3, 4, 5,
    4, 5, 3,
    3, 3, 3,
    2, 3, 1,
    4, 4, 4,
    4, 2, 1,
    5, 4, 5,
    3, 3, 5,
    2, 3, 4,
    5, 5, 5
  )

usethis::use_data(gwet4_table3.20, overwrite = TRUE)
