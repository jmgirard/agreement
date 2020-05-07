# page 359
gwet4_table12.8 <-
  tibble::tribble(
    ~Subject, ~Judge1, ~Judge2, ~Rank1, ~Rank2,
    1, 9, 1, 5, 2,
    2, 8, 1, 3.5, 2,
    3, 8, 4, 3.5, 5,
    4, 7, 1, 1.5, 2,
    5, 10, 5.8, 6, 6,
    6, 7, 2, 1.5, 4
  )

usethis::use_data(gwet4_table12.8, overwrite = TRUE)
