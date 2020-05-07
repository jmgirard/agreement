gwet4_table2.1 <-
  matrix(
    c(40, 4, 4, 17, 6, 25, 2, 13, 4, 1, 21, 12, 15, 5, 9, 45),
    nrow = 4,
    ncol = 4,
    dimnames = list(
      c("C_Schizo", "C_Bipolar", "C_Depress", "C_Other"),
      c("R_Schizo", "R_Bipolar", "R_Depress", "R_Other")
    )
  )
usethis::use_data(gwet4_table2.1, overwrite = TRUE)
