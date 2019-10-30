
calc_icc <- function(.data) {

  m <- as.matrix(stats::na.omit(.data))

  n_objects <- nrow(m)
  n_raters <- ncol(m)

  ss_total <- stats::var(as.numeric(m)) * (n_objects * n_raters - 1)
  ms_object <- stats::var(apply(m, 1, mean)) * n_raters
  ms_within <- sum(apply(m, 1, stats::var) / n_objects)
  ms_rater <- stats::var(apply(m, 2, mean)) * n_objects
  ms_resid <- (ss_total - ms_object * (n_objects - 1) - ms_rater * (n_raters - 1)) /
    ((n_objects - 1) * (n_raters - 1))

  icc_1 <- (ms_object - ms_within) / (ms_object + ms_within * (n_raters - 1))

  icc_k <- (ms_object - ms_within) / ms_object

  icc_c1 <- (ms_object - ms_resid) / (ms_object + ms_resid * (n_raters - 1))

  icc_ck <- (ms_object - ms_resid) / (ms_object)

  icc_a1 <- (ms_object - ms_resid) /
    (ms_object + ms_resid * (n_raters - 1) +
       (n_raters / n_objects) * (ms_rater - ms_resid))

  icc_ak <- (ms_object - ms_resid) /
    (ms_object + (ms_rater + ms_resid) / n_objects)

  if (format == "long") {
    out <- tibble::tribble(
      ~Approach, ~Model, ~Type, ~Unit, ~Estimate,
      "ICC_1", "One-way", "Agreement", "Single Measure", icc_1,
      "ICC_k", "One-way", "Agreement", "Average Measure", icc_k,
      "ICC_A1", "Two-way", "Agreement", "Single Measure", icc_a1,
      "ICC_Ak", "Two-way", "Agreement", "Average Measure", icc_ak,
      "ICC_C1", "Two-way", "Consistency", "Single Measure", icc_c1,
      "ICC_Ck", "Two-way", "Consistency", "Average Measure", icc_ck,
    )
  } else if (format == "wide") {
    out <- tibble::tibble(
      ICC_1 = icc_1,
      ICC_k = icc_k,
      ICC_A1 = icc_a1,
      ICC_Ak = icc_ak,
      ICC_C1 = icc_c1,
      ICC_Ck = icc_ck,
    )
  }

  out

}
