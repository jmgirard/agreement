test_that("Results Match Gwet Example 3.2", {

  # Gwet (2014) pp. 84 (plus errata)

  data("gwet4_table3.5_long")
  expect_warning(
    d <- prep_data_cat(
      .data = gwet4_table3.5_long,
      object = Object,
      rater = Rater,
      score = Score,
      categories = factor(LETTERS[1:3], ordered = TRUE),
      weighting = "quadratic"
    )
  )

  k_res <- calc_kappa(d$ratings, d$categories, d$weight_matrix, "objects")
  expect_equivalent(round(k_res,  4), c(0.9375, 0.7194, 0.7772))

  g_res <- calc_gamma(d$ratings, d$categories, d$weight_matrix, "objects")
  expect_equivalent(round(g_res,  4), c(0.9375, 0.6405, 0.8261))

  pi_res <- calc_pi(d$ratings, d$categories, d$weight_matrix, "objects")
  expect_equivalent(round(pi_res, 4), c(0.9375, 0.7314, 0.7673))

  a_res <- calc_alpha(d$ratings, d$categories, d$weight_matrix, "kripp")
  expect_equivalent(round(a_res, 4), c(0.9414, 0.7578, 0.7581))

  s_res <- calc_s(d$ratings, d$categories, d$weight_matrix, "objects")
  expect_equivalent(round(s_res, 4), c(0.9375, 0.6667, 0.8125))

})
