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
    "\nRaters = \t", object$details$n_raters,
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
  print(m, print.gap = 3L, na.print = "")
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
  a <- length(object$approach)
  index <- dplyr::case_when(
    which == "Observed" ~ 1,
    which == "Expected" ~ 2,
    which == "Adjusted" ~ 3
  )
  out <- matrix(NA, nrow = a, ncol = 2)
  rownames(out) <- paste0(object$approach, " ", which)
  colnames(out) <- sprintf("%.1f %%", c((1 - level) / 2, 1 - (1 - level) / 2) * 100)
  distributions <- object$boot_results$t[, seq(from = index, to = ncol(object$boot_results$t), by = 3)]

  if (a == 1) {
    out[, 1] <- stats::quantile(distributions, probs = (1 - level) / 2)
    out[, 2] <- stats::quantile(distributions, probs = 1 - (1 - level) / 2)
  } else {
    out[, 1] <- apply(distributions, MARGIN = 2, stats::quantile, probs = (1 - level) / 2)
    out[, 2] <- apply(distributions, MARGIN = 2, stats::quantile, probs = 1 - (1 - level) / 2)
  }

  out
}

#' @export
plot.agreement_cai <- function(object,
                               fill = "lightblue",
                               .width = 0.95,
                               base_size = 10,
                               size = 2,
                               ...) {

  distributions <- object$boot_results$t
  colnames(distributions) <- paste0(
    rep(object$approach, each = 3),
    c("_Observed", "_Expected", "_Adjusted")
  )

  plot_data <-
    tibble::as_tibble(distributions) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Approach",
      values_to = "Estimate"
    ) %>%
    tidyr::separate(
      col = "Approach",
      into = c("Approach", "Term"),
      sep = "_"
    ) %>%
    dplyr::mutate(
      Term = factor(Term, levels = c("Observed", "Expected", "Adjusted"),
                    labels = c("Raw Observed Agreement", "Expected Chance Agreement", "Chance-Adjusted Agreement"))
    )

  out <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = Estimate, y = 0)) +
    ggplot2::facet_grid(Approach ~ Term, switch = "y") +
    tidybayes::geom_halfeyeh(
      point_interval = tidybayes::mean_qi,
      fill = fill,
      .width = .width,
      size = size,
      ...
    ) +
    ggplot2::scale_x_continuous(NULL, breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-0.25, 1)) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 180))

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
