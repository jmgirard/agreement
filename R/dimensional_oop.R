# S3 Constructor for cai class
new_icc <- function(Object = double(),
                    Rater = double(),
                    Interaction = double(),
                    Residual = double(),
                    Intra_ICC = double(),
                    Inter_ICC = double(),
                    boot_results = list(),
                    formulation = list(),
                    details = list(),
                    call = rlang::quo(),
                    ...) {

  new_s3_scalar(
    Object = Object,
    Rater = Rater,
    Interaction = Interaction,
    Residual = Residual,
    Intra_ICC = Intra_ICC,
    Inter_ICC = Inter_ICC,
    boot_results = boot_results,
    formulation = formulation,
    details = details,
    call = call,
    ...,
    class = "agreement_icc"
  )

}

# Print method for objects of icc class
#' @export
print.agreement_icc <- function(x, digits = 3, ci = TRUE, level = 0.95, ...) {

  assert_that(digits == 0 || is.count(digits))
  assert_that(is.flag(ci))
  assert_that(level > 0, level < 1)

  with(x$formulation, cat(
    "\nIntraclass Correlation Coefficient\n",
    "(Model ", model, ", ", tools::toTitleCase(type), ", ",
    ifelse(k == 1, "Single Rater)", paste0("Average of ", k, " Raters)")),
    "\n\n",
    sep = ""
  ))

  m <- round(matrix(c(x$Intra_ICC, x$Inter_ICC), ncol = 1L), digits)
  rownames(m) <- c("Intra-Rater ICC", "Inter-Rater ICC")
  colnames(m) <- "Estimate"

  # Append Confidence Intervals for Adjusted Column if requested
  if (ci == TRUE) {
    ci <- round(stats::confint(x, level = level), digits)[5:6, 1:2]
    m <- cbind(m, ci)
  }

  # Remove rows with all missing values
  m <- m[!apply(is.na(m), 1, all), , drop = FALSE]

  # Print matrix
  print(m, print.gap = 3L, na.print = "")
  cat("\n")

}

# Summary method for objects of icc class
#' @export
summary.agreement_icc <- function(object, digits = 3, ci = TRUE, level = 0.95, ...) {

  # Validate inputs
  assert_that(digits == 0 || is.count(digits))
  assert_that(is.flag(ci))
  assert_that(is.number(level), level > 0, level < 1)

  # Print header
  with(object, cat(
    "\nIntraclass Correlation Coefficient Analysis Details\n\n",
    "Number of Objects  \t", details$n_objects, "\n",
    "Number of Raters   \t", details$n_raters, "\n",
    "Number of Trials   \t", details$n_trials, "\n",
    "\n",
    "Score Missingness  \t", sprintf("%.3f %%",
      (100 * details$n_missing_scores) /
        (details$n_objects * details$n_raters * details$n_trials)
      ), "\n",
    "Score Number Range \t[", details$min_score, ", ", details$max_score, "]\n",
    "\n",
    "ICC Model          \tModel ", formulation$model, "\n",
    "ICC Type           \t", tools::toTitleCase(formulation$type), "\n",
    "ICC Index          \t", ifelse(formulation$k == 1, "Single Rater",
      paste0("Average of ", formulation$k, " Raters")), "\n",
    "\n",
    "Variance Component Estimates with Bootstrapped CIs\n\n",
    sep = ""
  ))

  # Create matrix containing basic results
  v <- with(object, c(Object, Rater, Interaction, Residual, Intra_ICC, Inter_ICC))
  m <- round(matrix(v, ncol = 1L), digits)
  rownames(m) <- c(
    "Object Variance",
    "Rater Variance",
    "Interaction (OxR) Variance",
    "Residual Variance",
    "Intra-Rater ICC",
    "Inter-Rater ICC"
  )
  colnames(m) <- c("Estimate")

  # Append Confidence Intervals for Adjusted Column if requested
  if (ci == TRUE) {
    ci <- round(stats::confint(object, level = level), digits)
    m <- cbind(m, ci)
  }

  vm <- m[1:4, ]
  cm <- m[5:6, ]

  # Remove rows with all missing values
  vm <- vm[!apply(is.na(vm), 1, all), , drop = FALSE]
  cm <- cm[!apply(is.na(cm), 1, all), , drop = FALSE]

  # Print matrix
  print(vm, print.gap = 3L, na.print = "")
  cat("\nICC Estimates with Bootstrapped CIs\n\n")
  print(cm, print.gap = 3L, na.print = "")
  cat("\n")

}

# confint method for objects of icc class
#' @export
confint.agreement_icc <- function(object, level = 0.95, ...) {

  # Validate inputs
  assert_that(is.number(level), level > 0, level < 1)

  # Prepare matrix
  out <- matrix(NA_real_, nrow = 6, ncol = 2)
  rownames(out) <- c(
    "Object Variance",
    "Rater Variance",
    "O-by-R Variance",
    "Residual Variance",
    "Intra-Rater ICC",
    "Inter-Rater ICC"
  )
  colnames(out) <- sprintf("%.1f %%", c((1 - level) / 2, 1 - (1 - level) / 2) * 100)
  out[, 1] <- apply(
    object$boot_results$t,
    MARGIN = 2,
    stats::quantile,
    probs = (1 - level) / 2,
    na.rm = TRUE
  )
  out[, 2] <- apply(
    object$boot_results$t,
    MARGIN = 2,
    stats::quantile,
    probs = 1 - (1 - level) / 2,
    na.rm = TRUE
  )

  out
}
