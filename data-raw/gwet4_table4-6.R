# page 122
gwet4_table4.6 <-
  tibble::tribble(
    ~Rater1, ~Rater2,
    NA, 1,
    1.5, 1.5,
    2, 2,
    1, 1,
    0.5, 0.5,
    2, 2.5,
    0.5, 1,
    0.5, 0.5,
    0.5, NA,
    0.5, 0.5,
    2, 0.5,
    1.5, 2
  )

usethis::use_data(gwet4_table4.6, overwrite = TRUE)
