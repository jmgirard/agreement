# page 286
gwet4_table10.4 <-
  tibble::tribble(
    ~Subject, ~Judge1, ~Judge2, ~Judge3, ~Judge4,
    1, 6, 1, 3, 2,
    1, 6.5, 3, 3, 4,
    1, 4, 3, 5.5, 4,
    5, 10, 5, 6, 9,
    5, 9, 4.5, 5, 9,
    5, 9.5, 4, 6.6, 8,
    4, 6, 2, 4, 7,
    4, 7, 1, 3, 6,
    4, 8, 2.5, 4, 5,
    2, 9, 2, 5, 8,
    2, 7, 1, 2, 6,
    2, 8, 2, 2, 7,
    3, 10, 5, 6, 9,
    3, 7, 4, 6, 5,
    3, 8, 4, 6, 8
  )

usethis::use_data(gwet4_table10.4, overwrite = TRUE)
