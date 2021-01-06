test_that("coefficients match Gwet for interval", {
  # Gwet (2014) pp. 89 - 90

  data(interval)
  d <- prep_data_cat(
    .data = interval,
    object = Object,
    rater = Rater,
    score = Score,
    weighting = "quadratic",
    agreement = "objects"
  )

  k_res <- calc_kappa(d$ratings, d$categories, d$weight_matrix, "objects")
  expect_equivalent(round(k_res, 4), c(0.9206, 0.8314, 0.5290))

  s_res <- calc_s(d$ratings, d$categories, d$weight_matrix, "objects")
  expect_equivalent(round(s_res, 4), c(0.9206, 0.7500, 0.6823))

  pi_res <- calc_pi(d$ratings, d$categories, d$weight_matrix, "objects")
  expect_equivalent(round(pi_res, 4), c(0.9206, 0.8377, 0.5107))

  g_res <- calc_gamma(d$ratings, d$categories, d$weight_matrix, "objects")
  expect_equivalent(round(g_res, 4), c(0.9206, 0.6462, 0.7755))

  a_res <- calc_alpha(d$ratings, d$categories, d$weight_matrix, "kripp")
  expect_equivalent(round(a_res, 4), c(0.9364, 0.8336, 0.6180))

})
