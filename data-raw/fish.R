## code to prepare `fish` dataset goes here

fish <- tibble::tribble(
  ~A, ~B, ~C, ~D, ~E,
  2, 2, 3, 2, 2,
  2, 2, 2, 2, 2,
  2, NA, 2, 2, 1,
  1, 2, 2, 2, 2
)

usethis::use_data(fish, overwrite = TRUE)
