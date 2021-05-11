test_that("irsq equals what was reported in paper", {

  # van Oest, R. (2019). A new coefficient of interrater agreement: The
  # challenge of highly unequal category proportions. Psychological Methods,
  # 24(4), 439-451. https://doi.org/10/ggbk3f
  #
  # Data from Table 4 and Expectations from Table 5

  ex1 <- matrix(c(81, 9, 9, 1), nrow = 2, ncol = 2)
  long <- table_to_long(ex1)
  d <- prep_data_cat(
    .data = long,
    object = Object,
    rater = Rater,
    score = Score,
    weighting = "identity"
  )
  result <- calc_irsq(d$ratings, d$categories, d$weight_matrix, "pairs", c(1, 1))
  expect_equal(round(result[[3]], 3), 0.034)

  ex2a <- matrix(c(118, 2, 5, 0), nrow = 2, ncol = 2)
  long <- table_to_long(ex2a)
  d <- prep_data_cat(
    .data = long,
    object = Object,
    rater = Rater,
    score = Score,
    weighting = "identity"
  )
  result <- calc_irsq(d$ratings, d$categories, d$weight_matrix, "pairs", c(1, 1))
  expect_equal(round(result[[3]], 3), 0.089)

  ex2b <- ex2a * 4
  long <- table_to_long(ex2b)
  d <- prep_data_cat(
    .data = long,
    object = Object,
    rater = Rater,
    score = Score,
    weighting = "identity"
  )
  result <- calc_irsq(d$ratings, d$categories, d$weight_matrix, "pairs", c(1, 1))
  expect_equal(round(result[[3]], 3), 0.004)

})
