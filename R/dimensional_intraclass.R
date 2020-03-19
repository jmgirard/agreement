#' Calculate intraclass correlation coefficients for reliability
#'
#' Analyze intra- and inter-rater reliability of dimensional data using various
#' intraclass correlation coefficients. These coefficients are implemented using
#' a generalized approach that accommodates missing data and multiple trials.
#'
#' @param .data Required. A data frame containing variables identifying and
#'   quantifying each observation, in a "tall" format.
#' @param model Optional. A string indicating which ICC model to use, according
#'   to Gwet's (2014) typology. Options include "1A", "1B", "2", "2A", "3", and
#'   "3A". (default = \code{"1A"})
#' @param type Optional. A string indicating which ICC type to use, according to
#'   McGraw & Wong's (1999) typology. Options include "agreement" and
#'   "consistency". (default = \code{"agreement"})
#' @param unit Optional. A string indicating which unit of analysis to use.
#'   Options include "single" for the reliability of a single randomly chosen
#'   rater or "average" for the reliability of the average of all raters.
#'   (default = \code{"single"})
#' @param object Optional. The name of the variable in \code{.data} identifying
#'   the object of measurement for each observation, in non-standard evaluation
#'   without quotation marks. (default = \code{Object})
#' @param rater Optional. The name of the variable in \code{.data} identifying
#'   the rater or source of measurement for each observation, in non-standard
#'   evaluation without quotation marks. (default = \code{Rater})
#' @param score Optional. The name of the variable in \code{.data} containing
#'   the dimensional score or rating for each observation, in non-standard
#'   evaluation without quotation marks. (default = \code{Score})
#' @param trial Optional. The name of the variable in \code{.data} identifying
#'   the time or trial of measurement for each observation, in non-standard
#'   evaluation without quotation marks. (default = \code{NULL})
#' @param bootstrap Optional. A non-negative integer indicating how many
#'   bootstrap resamples to use in estimating confidence intervals. Set to
#'   \code{0} to forgo bootstrapping. (default = \code{2000})
#' @param warnings Optional. A logical value indicating whether warnings should
#'   be generated and displayed during this analysis. (default = \code{TRUE})
#' @return A list object of class "agreement_icc" with the following elements:
#'   \item{Object}{The estimated object variance} \item{Rater}{The estimated
#'   rater variance} \item{Interaction}{The estimated object-by-variance
#'   interaction variance} \item{Residual}{The estimated residual variance}
#'   \item{Intra_ICC}{The estimated intra-rater reliability}
#'   \item{Inter_ICC}{The estimated inter-rater reliability}
#'   \item{boot_results}{The bootstrap results from the boot package}
#'   \item{formulation}{The model, type, and k arguments} \item{details}{A list
#'   containing counts and data from the analysis} \item{call}{The function call
#'   that produced this output}
#' @export
dim_icc <- function(.data,
                    model = c("1A", "1B", "2", "2A", "3", "3A"),
                    type = c("agreement", "consistency"),
                    unit = c("single", "average"),
                    object = Object,
                    rater = Rater,
                    score = Score,
                    trial = NULL,
                    bootstrap = 2000,
                    warnings = TRUE) {

  # Validate inputs
  assert_that(is.data.frame(.data) || is.matrix(.data))
  model <- match.arg(model)
  type <- match.arg(type)
  unit <- match.arg(unit)
  assert_that(is.count(k))
  assert_that(bootstrap == 0 || is.count(bootstrap),
    msg = "The 'bootstrap' argument must be a non-negative integer.")
  assert_that(is.flag(warnings))

  # Prepare .data for analysis
  d <- prep_data_dim(.data, {{object}}, {{rater}}, {{score}}, {{trial}})

  if (unit == "single") {
    k <- 1
  } else {
    k <- d$n_raters
  }

  # Prepare empty output in case of errors
  out <- new_icc(
    Object = NA_real_,
    Rater = NA_real_,
    Interaction = NA_real_,
    Residual = NA_real_,
    Intra_ICC = NA_real_,
    Inter_ICC = NA_real_,
    boot_results = list(t = matrix(NA, nrow = 1, ncol = 6), t0 = rep(NA, 6)),
    formulation = list(model = model, type = type, unit = unit, k = k),
    details = d,
    call = match.call()
  )

  # Warn about bootstrapping samples with less than 20 objects
  if (d$n_objects < 20 && bootstrap > 0 && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

  # Error with impossible combinations of arguments
  if (type == "consistency" && (model == "1A" || model == "1B")) {
    stop("Consistency ICCs are not possible for models 1A and 1B.")
  }

  # Warn about there being fewer than 2 raters
  if (d$n_raters < 2) {
    if (warnings == TRUE) {
      warning("Only a single rater was observed. Returning NA.")
    }
    return(out)
  }
  #TODO: Check on intra-rater reliability with one rater - is it possible?

  if (d$n_objects < 2) {
    if (warnings == TRUE) {
      warning("Only a single object with scores was observed. Returning NA.")
    }
    return(out)
  }

  # Create function to perform bootstrapping
  boot_function <- function(codes, index, fun, k) {
    resample <- codes[index, , , drop = FALSE]
    bsr <- fun(ratings = resample, k = k)
    bsr
  }

  # Build function name for the requested ICC
  fun <- eval(
    parse(
      text = paste0("calc_icc_", model, ifelse(type == "agreement", "_A", "_C"))
    )
  )

  # Calculate the bootstrap results
  boot_results <-
    boot::boot(
      data = d$ratings,
      statistic = boot_function,
      R = bootstrap,
      fun = fun,
      k = k
    )

  # Construct icc class output object
  out <- new_icc(
    Object = boot_results$t0[[1]],
    Rater = boot_results$t0[[2]],
    Interaction = boot_results$t0[[3]],
    Residual = boot_results$t0[[4]],
    Intra_ICC = boot_results$t0[[5]],
    Inter_ICC = boot_results$t0[[6]],
    boot_results = boot_results,
    formulation = list(model = model, type = type, unit = unit, k = k),
    details = d,
    call = match.call()
  )

  out
}

# Functions to calculate different ICC estimates --------------------------

# Agreement ICC under Model 1A
# Model 1A applies when each object is rated by a different group of raters
calc_icc_1A_A <- function(ratings, k = 1) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model1(ratings, "A", n, s)

  v2 <- lapply(v, nonneg)

  # Inter-Rater ICC estimate
  inter_icc <- v2$object / (v2$object + v2$residual / k)

  # Create and label output vector
  out <- c(
    Object = v$object,
    Rater = NA_real_,
    Interaction = NA_real_,
    Residual = v$residual,
    Intra_ICC = NA_real_,
    Inter_ICC = inter_icc
  )

  out
}

# Agreement ICC under Model 1B
# Model 1B applies when each rater rates a different group of objects
calc_icc_1B_A <- function(ratings, ...) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model1(ratings, "B", n, s)

  v2 <- lapply(v, nonneg)

  # Intra-Rater ICC estimate
  intra_icc <- v2$rater / (v2$rater + v2$residual)

  # Create and label output vector
  out <- c(
    Object = NA_real_,
    Rater = v$rater,
    Interaction = NA_real_,
    Residual = v$residual,
    Intra_ICC = intra_icc,
    Inter_ICC = NA_real_
  )

  out
}

# Agreement ICC under Model 2
# Model 2 applies when both raters and objects are random, includes interaction
# Model 2 requires that at least some objects have multiple trials per rater
calc_icc_2_A <- function(ratings, k = 1) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model2(ratings, n, s)

  v2 <- lapply(v, nonneg)

  # Inter-Rater ICC estimate
  inter_icc <- v2$object /
    (v2$object + (v2$rater + v2$interaction + v2$residual) / k)

  # Intra-Rater ICC estimate
  # TODO: Check whether to divide part of denominator by k
  intra_icc <- (v2$object + v2$rater + v2$interaction) /
    (v2$object + v2$rater + v2$interaction + v2$residual)

  # Create and label output vector
  out <- c(
    Object = v$object,
    Rater = v$rater,
    Interaction = v$interaction,
    Residual = v$residual,
    Intra_ICC = intra_icc,
    Inter_ICC = inter_icc
  )

  out
}

# Agreement ICC under Model 2A
# Model 2A applies when both raters and objects are random, excludes interaction
# Model 2A can be used with single or multiple trials per rater
calc_icc_2A_A <- function(ratings, k = 1) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model2A(ratings, n, s)

  v2 <- lapply(v, nonneg)

  # Inter-Rater ICC estimate
  inter_icc <- v2$object / (v2$object + (v2$rater + v2$residual) / k)

  # Intra-Rater ICC estimate
  # TODO: Check whether to divide part of denominator by k
  intra_icc <- (v2$object + v2$rater) / (v2$object + v2$rater + v2$residual)

  # Create and label output vector
  out <- c(
    Object = v$object,
    Rater = v$rater,
    Interaction = NA_real_,
    Residual = v$residual,
    Intra_ICC = intra_icc,
    Inter_ICC = inter_icc
  )

  out
}

# Consistency ICC under Model 2
# Model 2 applies when both raters and objects are random, includes interaction
# Model 2 requires that at least some objects have multiple trials per rater
calc_icc_2_C <- function(ratings, k = 1) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model2(ratings, n, s)

  v2 <- lapply(v, nonneg)

  # Inter-Rater ICC estimate
  inter_icc <- v2$object /
    (v2$object + (v2$interaction + v2$residual) / k)

  # Create and label output vector
  out <- c(
    Object = v$object,
    Rater = NA_real_,
    Interaction = v$interaction,
    Residual = v$residual,
    Intra_ICC = NA_real_,
    Inter_ICC = inter_icc
  )

  out
}

# Consistency ICC under Model 2A
# Model 2A applies when both raters and objects are random, excludes interaction
calc_icc_2A_C <- function(ratings, k = 1) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model2A(ratings, n, s)

  v2 <- lapply(v, nonneg)

  # Inter-Rater ICC estimate
  inter_icc <- v2$object / (v2$object + v2$residual / k) #TODO: Check formula

  # Create and label output vector
  out <- c(
    Object = v$object,
    Rater = NA_real_,
    Interaction = NA_real_,
    Residual = v$residual,
    Intra_ICC = NA_real_,
    Inter_ICC = inter_icc
  )

  out
}

# TODO: Models 3 and 3A


# Functions to calculate ICC components -----------------------------------

icc_counts <- function(ratings) {

  n <- list()

  # Count of objects
  n$objects <- dim(ratings)[[1]]

  # Count of raters
  n$raters <- dim(ratings)[[2]]

  # Count of trials per object-rater combination
  n$trials_or <- apply(is.finite(ratings), MARGIN = 1:2, FUN = sum)

  # Count of trials per object
  n$trials_o <- rowSums(n$trials_or)

  # Count of trials per rater
  n$trials_r <- colSums(n$trials_or)

  # Count of all trials
  n$trials <- sum(n$trials_or)

  n
}

icc_sums <- function(ratings, n = NULL) {

  if (is_null(n)) {n <- icc_counts(ratings)}

  s <- list()

  # Sum all trials per object-rater combination
  ysum_or <- apply(ratings, MARGIN = 1:2, FUN = sum, na.rm = TRUE)

  # Sum of all ratings per object
  ysum_o <- apply(ratings, MARGIN = 1, FUN = sum, na.rm = TRUE)

  # Sum of all ratings per rater
  ysum_r <- apply(ratings, MARGIN = 2, FUN = sum, na.rm = TRUE)

  # Total sum of ratings
  s$T_y <- sum(ysum_or, na.rm = TRUE)

  # Total sum of squared ratings
  s$T_ysq <- sum(ratings^2, na.rm = TRUE)

  # Sum of all object's averaged squared ratings
  s$T_objects <- sum(ysum_o^2 / n$trials_o, na.rm = TRUE)

  # Sum of all raters' average squared ratings
  s$T_raters <- sum(ysum_r^2 / n$trials_r, na.rm = TRUE)

  # Sum of all object-rater combinations' average squared ratings
  s$T_interaction <- sum(ysum_or^2 / n$trials_or, na.rm = TRUE)

  # Sum of all average ratings
  s$T_musq <- n$trials * mean(ratings, na.rm = TRUE)^2

  # Number of object-rater combinations for which one or more trials exist
  s$lambda_0 <- sum(n$trials_or > 0)

  # Expected number of trials per object
  s$k_r <- sum(n$trials_or^2 / n$trials_r)

  # Expected number of trails per rater
  s$k_o <- sum(n$trials_or^2 / n$trials_o)

  # TODO: Write informative comment
  s$kprime_o <- sum(n$trials_o^2) / n$trials
  s$kprime_r <- sum(n$trials_r^2) / n$trials
  s$kprime_or <- sum(n$trials_or^2) / n$trials
  s$Tprimesq_ysq <- s$T_y^2 / n$trials

  # TODO: Write informative comment
  s$lambda_1 <- (n$trials - s$kprime_o) / (n$trials - s$k_r)
  s$lambda_2 <- (n$trials - s$kprime_r) / (n$trials - s$k_o)

  s
}

icc_model1 <- function(ratings, type = c("A", "B"), n = NULL, s = NULL) {

  type <- match.arg(type)
  if (is_null(n)) {n <- icc_counts(ratings)}
  if (is_null(s)) {s <- icc_sums(ratings, n)}

  v <- list()

  if (type == "A") {
    v$residual <- (s$T_ysq - s$T_objects) / (n$trials - n$objects)
    v$object <- (s$T_objects - (s$T_y^2 / n$trials) - v$residual * (n$objects - 1)) / (n$trials - s$k_r)
  } else if (type == "B") {
    v$residual <- (s$T_ysq - s$T_raters) / (n$trials - n$raters)
    v$rater <- (s$T_raters - (s$T_y^2 / n$trials) - v$residual * (n$raters - 1)) / (n$trials - s$k_o)
  }

  v
}

icc_model2 <- function(ratings, n = NULL, s = NULL) {

  if (is_null(n)) {n <- icc_counts(ratings)}
  if (is_null(s)) {s <- icc_sums(ratings, n)}

  v <- list()

  v$residual <- (s$T_ysq - s$T_interaction) / (n$trials - s$lambda_0)

  delta_r <- (s$T_interaction - s$T_raters - v$residual * (s$lambda_0 - n$raters)) / (n$trials - s$k_r)
  delta_o <- (s$T_interaction - s$T_objects - v$residual * (s$lambda_0 - n$objects)) / (n$trials - s$k_o)

  v$interaction <-
    (delta_r * (n$trials - s$kprime_o) +
      delta_o * (s$k_o - s$kprime_r) -
      (s$T_objects - s$Tprimesq_ysq - v$residual * (n$objects - 1))) /
      (n$trials - s$kprime_o - s$kprime_r + s$kprime_or)

  v$rater <- delta_o - v$interaction
  v$object <- delta_r - v$interaction

  v
}

icc_model2A <- function(ratings, n = NULL, s = NULL) {

  if (is_null(n)) {n <- icc_counts(ratings)}
  if (is_null(s)) {s <- icc_sums(ratings, n)}

  v <- list()

  v$residual <- (s$lambda_2 * (s$T_ysq - s$T_objects) + s$lambda_1 * (s$T_ysq - s$T_raters) - (s$T_ysq - s$T_musq)) /
    (s$lambda_2 * (n$trials - n$objects) + s$lambda_1 * (n$trials - n$raters) - (n$trials - 1))

  v$object <- (s$T_ysq - s$T_raters - v$residual * (n$trials - n$raters)) / (n$trials - s$k_r)

  v$rater <- (s$T_ysq - s$T_objects - v$residual * (n$trials - n$objects)) / (n$trials - s$k_o)

  v
}
