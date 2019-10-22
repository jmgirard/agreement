# S3 Constructor for cai class
new_cai <- function(approach = character(),
                    observed = double(),
                    expected = double(),
                    adjusted = double(),
                    boot_results = list(),
                    details = list(),
                    call = rlang::quo(),
                    ...) {

  new_s3_scalar(
    approach = approach,
    observed = observed,
    expected = expected,
    adjusted = adjusted,
    boot_results = boot_results,
    details = details,
    call = call,
    ...,
    class = "agreement_cai"
  )

}

# Print method for objects of cai class
#' @export
print.agreement_cai <- function(x, digits = 3, ...) {

  assert_that(digits == 0 || is.count(digits))

  # Print function call
  cat(
    "\nCall:\n",
    paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n\n",
    "Chance-Adjusted Categorical Agreement\n\n",
    sep = ""
  )

  v <- c(x$observed, x$expected, x$adjusted)
  m <- round(matrix(v, ncol = 3L), digits)
  rownames(m) <- paste0(x$details$weighting, " ", x$approach)
  colnames(m) <- c("Observed", "Expected", "Adjusted")
  print.default(m, print.gap = 3L, na.print = "")
  cat("\n")

}

# Summary method for objects of cai class
#' @export
summary.agreement_cai <- function(object, digits = 3, ci = TRUE, level = 0.95, ...) {

  # Validate inputs
  assert_that(digits == 0 || is.count(digits))
  assert_that(is.flag(ci))
  assert_that(is.number(level), level > 0, level < 1)

  # Print function call and header
  cat(
    "\nCall:\n",
    paste(deparse(object$call), sep = "\n", collapse = "\n"),
    "\n\n",
    "Objects = \t", object$details$n_objects,
    " Raters = \t", object$details$n_raters,
    "\nCategories = \t{", paste(object$details$categories, collapse = ", "), "}",
    "\nWeighting = \t", object$details$weighting,
    "\n\n",
    "Chance-Adjusted Categorical Agreement with Bootstrapped CIs\n\n",
    sep = ""
  )

  # Create matrix containing basic results
  v <- c(object$observed, object$expected, object$adjusted)
  m <- round(matrix(v, ncol = 3L), digits)
  rownames(m) <- object$approach
  colnames(m) <- c("Observed", "Expected", "Adjusted")

  # Append Confidence Intervals for Adjusted Column if requested
  if (ci == TRUE) {
    ci <- round(confint(object, which = "Adjusted"), digits)
    m <- cbind(m, ci)
  }

  # Print matrix
  print.default(m, print.gap = 3L, na.print = "")
  cat("\n")

}

# confint method for objects of cai class
confint.agreement_cai <- function(object,
                                  which = c("Observed", "Expected", "Adjusted"),
                                  level = 0.95,
                                  ...) {

  # Validate inputs
  which <- match.arg(which)
  assert_that(is.number(level), level > 0, level < 1)

  # Prepare matrix
  index <- dplyr::case_when(
    which == "Observed" ~ 1,
    which == "Expected" ~ 2,
    which == "Adjusted" ~ 3
  )
  out <- matrix(NA, nrow = length(object$approach), ncol = 2)
  rownames(out) <- paste0(object$approach, " ", which)
  colnames(out) <- sprintf("%.1f %%", c((1 - level) / 2, 1 - (1 - level) / 2) * 100)
  distributions <- object$boot_results$t[, seq(from = index, to = ncol(object$boot_results$t), by = 3)]
  out[, 1] <- apply(distributions, MARGIN = 2, stats::quantile, probs = (1 - level) / 2)
  out[, 2] <- apply(distributions, MARGIN = 2, stats::quantile, probs = 1 - (1 - level) / 2)

  out
}

#' @export
plot.agreement_cai <- function(object,
                               which = c("Observed", "Expected", "Adjusted"),
                               level = 0.95) {

  which <- match.arg(which)
  assert_that(is.number(level), level > 0, level < 1)

  index <- dplyr::case_when(
    which == "Observed" ~ 1,
    which == "Expected" ~ 2,
    which == "Adjusted" ~ 3
  )

  distributions <- object$boot_results$t[, seq(from = index, to = ncol(object$boot_results$t), by = 3)]
  colnames(distributions) <- object$approach
  df <- tibble::as_tibble(distributions)
  df_long <- tidyr::pivot_longer(df, cols = dplyr::everything(), names_to = "Approach", values_to = "Estimate")
  ci <- confint(object, which, level)
  df_ci <- tibble(Approach = object$approach, LCI = ci[, 1], UCI = ci[, 2])

  out <- ggplot2::ggplot(data = df_long, ggplot2::aes(x = Estimate)) +
    ggplot2::facet_wrap(~Approach) + ggplot2::geom_density(fill = "white") +
    ggplot2::geom_vline(data = df_ci, ggplot2::aes(xintercept = LCI)) +
    ggplot2::geom_vline(data = df_ci, ggplot2::aes(xintercept = UCI)) +
    ggplot2::scale_x_continuous(NULL, breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_cartesian(xlim = c(0, 1))

  out
}

#' @export
tidy.agreement_cai <- function(x, ...) {
  a <- length(x$approach)
  out <- tibble(
    approach = rep(x$approach, times = 3),
    weighting = rep(x$details$weighting, times = a * 3),
    term = rep(c("Observed", "Expected", "Adjusted"), each = a),
    estimate = c(x$observed, x$expected, x$adjusted)
  )

  out
}
