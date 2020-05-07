# page 187
gwet4_table7.1 <-
  tibble::tribble(
    ~R1, ~R2, ~R3, ~R4,
    9, 2, 5, 8,
    6, 1, 3, 2,
    8, 4, 6, 8,
    7, 1, 2, 6,
    10, 5, 6, 9,
    6, 2, 4, 7
  )

usethis::use_data(gwet4_table7.1, overwrite = TRUE)
