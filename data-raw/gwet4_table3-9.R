# page 90
gwet4_table3.9 <-
  tibble::tribble(
    ~Subject, ~L, ~K, ~W, ~B,
    "a.lycia", 1, 1.5, 1, NA,
    "a.milbe", 2, 2, 2, 2,
    "a.hegon", 0.5, 1, 1.5, 1.5,
    "a.oslar", 1, 1, 1, 1,
    "a.viali", 1, 1, 1, 1.5,
    "a.logan", NA, 1, 2.5, NA,
    "a.numit", 2.5, 2.5, 2.5, 2.5,
    "a.saraa", 1, 1, NA, 1,
    "a.sarat", NA, 1, 2, 1,
    "a.mormo", 1, 1, 0.5, 1,
    "a.celti", 1.5, 1.5, 1.5, 1.5,
    "a.clyto", 1, 1.5, 1, NA,
    "a.hiann", 1, 1, 1.5, NA,
    "b.phile", 1, 2, 2.5, 2,
    "b.alask", NA, 1, 1.5, 1,
    "b.taad", 0.5, 0.5, 0.5, 0.5
  )

usethis::use_data(gwet4_table3.9, overwrite = TRUE)
