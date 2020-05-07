# page 361
gwet4_table12.10 <-
  tibble::tribble(
    ~Subject, ~Judge1, ~Judge2, ~Judge3, ~Judge4,
    "A", 9, 2, 5, 8,
    "B", 6, 1, 3, 2,
    "C", 8, 4, 6, 8,
    "D", 7, 1, 2, 6,
    "E", 10, 5, 6, 9,
    "F", 6, 2, 4, 7
  )

usethis::use_data(gwet4_table12.10, overwrite = TRUE)
