test_that("coefficients match Gwet for unordered", {

  # Gwet (2014) pp. 55-56

  data(unordered)
  d <- prep_data_cat(unordered, Object, Rater, Score, letters[1:5], "identity")

  k_res <- calc_kappa(d$ratings, d$categories, d$weight_matrix)
  expect_equivalent(round(k_res, 4), c(0.8182, 0.2334, 0.7628))

  s_res <- calc_s(d$ratings, d$categories, d$weight_matrix)
  expect_equivalent(round(s_res, 4), c(0.8182, 0.2000, 0.7727))

  pi_res <- calc_pi(d$ratings, d$categories, d$weight_matrix)
  expect_equivalent(round(pi_res, 4), c(0.8182, 0.2387, 0.7612))

  g_res <- calc_gamma(d$ratings, d$categories, d$weight_matrix)
  expect_equivalent(round(g_res, 4), c(0.8182, 0.1903, 0.7754))

  a_res <- calc_alpha(d$ratings, d$categories, d$weight_matrix)
  expect_equivalent(round(a_res, 4), c(0.8227, 0.3091, 0.7434))

})
