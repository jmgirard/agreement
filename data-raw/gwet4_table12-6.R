# page 357
gwet4_table12.6 <-
  tibble::tribble(
    ~Subject, ~Judge1, ~Judge2, ~Rank1, ~Rank2,
    1, 9, 2.7, 5, 4,
    2, 6.6, 1.4, 2, 2,
    3, 8, 4, 4, 5,
    4, 7.1, 1, 3, 1,
    5, 10, 5.8, 6, 6,
    6, 6, 2, 1, 3
  )

usethis::use_data(gwet4_table12.6, overwrite = TRUE)
