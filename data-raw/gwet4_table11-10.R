# page 337
gwet4_table11.10 <-
  tibble::tribble(
    ~Place, ~JudgeA, ~JudgeB,
    1, "Good", "Good",
    2, "Good", "Good",
    3, "Good", "Good",
    4, "Good", "Bad",
    5, "Good", "Good",
    6,"Bad", "Bad",
    7, "Good", "Good",
    8, "Good", "Good",
    9, "Good", "Good",
    10, "Bad", "Bad",
    11, "Good", "Good",
    12, "Bad", "Bad"
  )

usethis::use_data(gwet4_table11.10, overwrite = TRUE)
