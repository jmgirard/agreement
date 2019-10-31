#' @export
dim_icc <- function(.data,
                    approach = c("1", "k", "A1", "Ak", "C1", "Ck"),
                    format = c("wide", "long"),
                    ...,
                    bootstrap = 2000,
                    warnings = TRUE) {

  # Validate inputs
  assert_that(is.data.frame(.data) || is.matrix(.data))
  approach <- match.arg(approach, several.ok = TRUE)
  approach <- unique(approach)
  format <- match.arg(format)
  assert_that(bootstrap == 0 || is.count(bootstrap))
  assert_that(is.flag(warnings))

  # Prepare .data for analysis
  if (format == "wide") {
    d <- prep_data_dim(.data)
  } else if (format == "long") {
    d <- prep_data_dim_long(.data, ...)
  }

  # Warn about bootstrapping samples with less than 20 objects
  if (d$n_objects < 20 && bootstrap > 0 && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

  d
}

# ICC for single score under Model 1A
# Model 1A applies when each object is rated by a different group of raters
calc_icc_1A_1 <- function(ratings) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model1(ratings, "A", n, s)

  v2 <- lapply(v, nonneg)

  # ICC estimate
  est <- v2$object / (v2$object + v2$residual)

  # Create and label output vector
  out <- c(Object = v$object, Residual = v$residual, Inter_ICC = est)

  out
}

calc_icc_1A_k <- function(ratings, k = NULL) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model1(ratings, "B", n, s)

  v2 <- lapply(v, nonneg)

  if (is_null(k)) {k <- n$raters}

  # ICC estimate
  est <- v2$object / (v2$object + v2$residual / k)

  # Create and label output vector
  out <- c(Object = v$object, Residual = v$residual, Inter_ICC = est)

  out
}

# ICC for single score under Model 1B
# Model 1B applies when each rater rates a different group of objects
calc_icc_1B_1 <- function(ratings) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model1(ratings, "B", n, s)

  v2 <- lapply(v, nonneg)

  # ICC estimate
  est <- v2$rater / (v2$rater + v2$residual)

  # Create and label output vector
  out <- c(Rater = v$rater, Residual = v$residual, Intra_ICC = est)

  out
}

# ICC for single score under Model 2
# Model 2 applies when both raters and objects are random
calc_icc_2_1 <- function(ratings) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model2(ratings, n, s)

  v2 <- lapply(v, nonneg)

  # Inter-Rater ICC estimate
  inter_icc <- v2$object / (v2$object + v2$rater + v2$interaction + v2$residual)
  intra_icc <- (v2$rater + v2$object + v2$interaction) /
    (v2$rater + v2$object + v2$interaction + v2$residual)

  # Intra-Rater ICC estimate

  # Create and label output vector
  out <- c(Object = v$object, Rater = v$rater, Interaction = v$interaction,
           Residual = v$residual, Inter_ICC = inter_icc, Intra_ICC = intra_icc)

  out
}

#
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

#
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

  # Number of object-rater combinations for which one or more trials exist
  s$lambda_0 <- sum(n$trials_or > 0)

  # Expected number of trials per object
  s$k_0 <- sum(n$trials_or^2 / n$trials_r)

  # Expected number of trails per rater
  s$k_1 <- sum(n$trials_or^2 / n$trials_o)

  # TODO: Write informative comment
  s$kprime_1 <- sum(n$trials_o^2) / n$trials
  s$kprime_2 <- sum(n$trials_r^2) / n$trials
  s$kprime_5 <- sum(n$trials_or^2) / n$trials
  s$Tprimesq_ysq <- s$T_y^2 / n$trials

  s
}

#
icc_model1 <- function(ratings, type = c("A", "B"), n = NULL, s = NULL) {

  type <- match.arg(type)
  if (is_null(n)) {n <- icc_counts(ratings)}
  if (is_null(s)) {s <- icc_sums(ratings, n)}

  v <- list()

  if (type == "A") {
    v$residual <- (s$T_ysq - s$T_objects) / (n$trials - n$objects)
    v$object <- (s$T_objects - (s$T_y^2 / n$trials) - v$residual * (n$objects - 1)) / (n$trials - s$k_0)
  } else if (type == "B") {
    v$residual <- (s$T_ysq - s$T_raters) / (n$trials - n$raters)
    v$rater <- (s$T_raters - (s$T_y^2 / n$trials) - v$residual * (n$raters - 1)) / (n$trials - s$k_1)
  }

  v
}

#
icc_model2 <- function(ratings, n = NULL, s = NULL) {

  if (is_null(n)) {n <- icc_counts(ratings)}
  if (is_null(s)) {s <- icc_sums(ratings, n)}

  v <- list()

  v$residual <- (s$T_ysq - s$T_interaction) / (n$trials - s$lambda_0)

  delta_r <- (s$T_interaction - s$T_raters - v$residual * (s$lambda_0 - n$raters)) / (n$trials - s$k_0)
  delta_o <- (s$T_interaction - s$T_objects - v$residual * (s$lambda_0 - n$objects)) / (n$trials - s$k_1)

  v$interaction <-
    (delta_r * (n$trials - s$kprime_1) +
      delta_o * (s$k_1 - s$kprime_2) -
      (s$T_objects - s$Tprimesq_ysq - v$residual * (n$objects - 1))) /
      (n$trials - s$kprime_1 - s$kprime_2 + s$kprime_5)

  v$rater <- delta_o - v$interaction
  v$object <- delta_r - v$interaction

  v
}
