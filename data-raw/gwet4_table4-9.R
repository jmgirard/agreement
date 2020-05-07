# page 125
gwet4_table4.9 <-
  tibble::tribble(
    ~Obs1, ~Obs2, ~Obs3, ~Obs4, ~Obs5,
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

usethis::use_data(gwet4_table4.9, overwrite = TRUE)
