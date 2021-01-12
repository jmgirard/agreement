
# new_cai -----------------------------------------------------------------

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

# print.agreement_cai -----------------------------------------------------

# Print method for objects of cai class
#' @method print agreement_cai
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


# summary.agreement_cai ---------------------------------------------------

# Summary method for objects of cai class
#' @method summary agreement_cai
#' @export
summary.agreement_cai <- function(object,
                                  digits = 3,
                                  ci = TRUE,
                                  ...) {

  # Validate inputs
  assert_that(digits == 0 || is.count(digits))
  assert_that(is.flag(ci))

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
    ci <- round(
      stats::confint(
        object,
        ...
        )[seq(3, length(object$approach) * 3, by = 3), ],
      digits
    )
    m <- cbind(m, ci)
  }

  # Print matrix
  print(m, print.gap = 3L, na.print = "")
  cat("\n")

}

# confint.agreement_cai ---------------------------------------------------

#' Calculate confidence intervals for chance-adjusted agreement indexes
#'
#' Calculate confidence intervals for chance-adjusted agreement indexes using
#' one of several approaches.
#'
#' @param object A chance-adjusted agreement or "cai" object.
#' @param level A single real number between 0 and 1 that indicates the
#'   confidence level or width of the confidence intervals.
#' @param type A single string indicating which type of bootstrap confidence
#'   interval to calculate. Options currently include "bca" for bias-corrected
#'   and accelerated bootstrap CIs, "perc" for bootstrap percentile CIs, "basic"
#'   for basic bootstrap CIs, and "norm" for normal approximation CIs.
#' @param ... Further arguments to be passed to \code{boot::boot.ci()}.
#' @method confint agreement_cai
#' @export
confint.agreement_cai <- function(object,
                                  level = 0.95,
                                  type = c("bca", "perc", "basic", "norm"),
                                  ...) {

  # Validate inputs
  assert_that(is.number(level), level > 0, level < 1)
  type <- match.arg(type, several.ok = FALSE)

  # Calculate CI bounds and store in list
  ci_list <-
    lapply(
      1:ncol(object$boot_results$t),
      function (i) safe_boot.ci(
        object$boot_results,
        level = level,
        type = type,
        index = i,
        ...
      )
    )

  # Convert list to matrix and add dimnames
  out <- matrix(unlist(ci_list), ncol = 2, byrow = TRUE)
  rownames(out) <-
    paste0(
      rep(object$approach, each = 3),
      " ",
      c("Observed", "Expected", "Adjusted")
    )
  colnames(out) <- c("lower", "upper")

  out
}

# plot.agreement_cai ------------------------------------------------------

#' @method plot agreement_cai
#' @export
plot.agreement_cai <- function(x,
                               fill = "lightblue",
                               .width = 0.95,
                               base_size = 10,
                               size = 2,
                               ...) {

  distributions <- x$boot_results$t
  colnames(distributions) <- paste0(
    rep(x$approach, each = 3),
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
                    labels = c("Raw Observed\nAgreement",
                               "Expected Chance\nAgreement",
                               "Chance-Adjusted\nAgreement"))
    )

  out <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = Estimate, y = 0)) +
    ggplot2::facet_grid(Approach ~ Term, switch = "y") +
    ggdist::stat_halfeye(
      point_interval = ggdist::mean_qi,
      fill = fill,
      .width = .width,
      size = size,
      normalize = "panels",
      ...
    ) +
    ggplot2::scale_x_continuous(NULL, breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-0.25, 1)) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 180))

    out
}

# tidy.agreement_cai ------------------------------------------------------

#' @inheritParams confint.agreement_cai
#' @method tidy agreement_cai
#' @export
tidy.agreement_cai <- function(x, se = FALSE, ...) {

  assert_that(is.flag(se))

  a <- length(x$approach)
  ci_vals <- stats::confint(x, ...)
  out <- tibble(
    approach = rep(x$approach, times = 3),
    weighting = rep(x$details$weighting, times = a * 3),
    term = rep(c("Observed", "Expected", "Adjusted"), each = a),
    estimate = c(x$observed, x$expected, x$adjusted),
    lower = ci_vals[, 1],
    upper = ci_vals[, 2]
  )

  if (se == TRUE) {
    out <- dplyr::mutate(
      out,
      se = apply(x$boot_results$t, MARGIN = 2, FUN = sd),
      .after = "estimate"
    )
  }

  out
}

# new_spa -----------------------------------------------------------------

# S3 Constructor for spa class
new_spa <- function(
  observed = double(),
  boot_results = list(),
  details = list(),
  call = rlang::quo(),
  ...) {

  new_s3_scalar(
    observed = observed,
    boot_results = boot_results,
    details = details,
    call = call,
    ...,
    class = "agreement_spa"
  )

}

# print.agreement_spa -----------------------------------------------------

# Print method for objects of spa class
#' @method print agreement_spa
#' @export
print.agreement_spa <- function(x, digits = 3, ...) {

  assert_that(digits == 0 || is.count(digits))

  # Print function call
  cat(
    "\nCall:\n",
    paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n\n",
    "Category-Specific Agreement\n\n",
    sep = ""
  )

  m <- round(matrix(x$observed, ncol = 1L), digits)
  rownames(m) <- paste0(x$details$categories)
  colnames(m) <- "Estimate"
  print.default(m, print.gap = 3L, na.print = "")
  cat("\n")

}

# confint.agreement_spa ---------------------------------------------------

# Confint method for objects of spa class
#' @method confint agreement_spa
#' @export
confint.agreement_spa <- function(object,
                                  level = 0.95,
                                  type = c("bca", "perc", "basic", "norm"),
                                  ...) {

  # Validate inputs
  assert_that(is.number(level), level > 0, level < 1)
  type <- match.arg(type, several.ok = FALSE)

  # Calculate CI bounds and store in list
  ci_list <-
    lapply(
      1:ncol(object$boot_results$t),
      function (i) safe_boot.ci(
          object$boot_results,
          level = level,
          type = type,
          index = i,
          ...
        )
      )

  # Convert list to matrix and add dimnames
  out <- matrix(unlist(ci_list), ncol = 2, byrow = TRUE)
  rownames(out) <- object$details$categories
  colnames(out) <- c("lower", "upper")

  out
}

# summary.agreement_spa ---------------------------------------------------

# Summary method for objects of spa class
#' @method summary agreement_spa
#' @export
summary.agreement_spa <- function(object, digits = 3, ci = TRUE, ...) {

  # Validate inputs
  assert_that(digits == 0 || is.count(digits))
  assert_that(is.flag(ci))

  # Print function call and header
  cat(
    "\nCall:\n",
    paste(deparse(object$call), sep = "\n", collapse = "\n"),
    "\n\n",
    "Objects = \t", object$details$n_objects,
    "\nRaters = \t", object$details$n_raters,
    "\nCategories = \t{", paste(object$details$categories, collapse = ", "), "}",
    "\n\n",
    "Category-Specific Agreement with Bootstrapped CIs\n\n",
    sep = ""
  )

  # Create matrix containing basic results
  m <- round(matrix(object$observed, ncol = 1L), digits)
  rownames(m) <- object$details$categories
  colnames(m) <- "Estimate"

  # Append Confidence Intervals for Adjusted Column if requested
  if (ci == TRUE) {
    ci <- round(stats::confint(object, ...), digits)
    m <- cbind(m, ci)
  }

  # Print matrix
  print(m, print.gap = 3L, na.print = "")
  cat("\n")

}

# Tidy method for objects of spa class
#' @method tidy agreement_spa
#' @export
tidy.agreement_spa <- function(x, ...) {
  a <- length(x$approach)
  ci_vals <- stats::confint(x, ...)
  out <- tibble(
    approach = "Specific Agreement",
    category = x$details$categories,
    estimate = x$observed,
    lower = ci_vals[, 1],
    upper = ci_vals[, 2]
  )

  out
}

# Plot method for objects of spa class
#' @method plot agreement_spa
#' @export
plot.agreement_spa <- function(x,
  fill = "lightblue",
  .width = 0.95,
  base_size = 10,
  size = 2,
  ...) {

  distributions <- x$boot_results$t
  colnames(distributions) <- paste0("Category = ", x$details$categories)

  plot_data <-
    tibble::as_tibble(distributions) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Category",
      values_to = "Estimate"
    ) %>%
    dplyr::mutate(
      Category = factor(Category, levels = colnames(distributions))
    ) %>%
    tidyr::drop_na()

  out <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = Estimate, y = 0)) +
    ggplot2::facet_wrap(~Category, ncol = 1) +
    ggdist::stat_halfeye(
      point_interval = ggdist::mean_qi,
      fill = fill,
      .width = .width,
      size = size,
      normalize = "panels",
      ...
    ) +
    ggplot2::scale_x_continuous("Category-Specific Agreement", breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-0.25, 1)) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 180))

  out
}
