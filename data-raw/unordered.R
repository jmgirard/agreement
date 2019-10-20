unordered <-
  tibble::tribble(
    ~R1, ~R2, ~R3, ~R4,
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

usethis::use_data(unordered, overwrite = TRUE)
