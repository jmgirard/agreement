# page 68 (table 2.26) and 131 (also table 5.1)
gwet4_table2.26 <-
  tibble::tribble(
    ~A, ~B, ~C, ~D,
    "a", "a", "a", "c",
    "a", "a", "b", "c",
    "a", "a", "b", "c",
    "a", "a", "c", "c",
    "a", "b", "a", "a",
    "b", "a", "a", "a",
    "b", "b", "b", "b",
    "b", "c", "b", "b",
    "c", "c", "b", "b",
    "c", "c", "c", "c"
  )

usethis::use_data(gwet4_table2.26, overwrite = TRUE)
