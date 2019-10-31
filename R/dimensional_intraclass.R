#' @export
dim_icc <- function(.data,
                    approach = c("1", "k", "A1", "Ak", "C1", "Ck"),
                    bootstrap = 2000,
                    warnings = TRUE) {

  # Validate inputs
  assert_that(is.data.frame(.data) || is.matrix(.data))
  approach <- match.arg(approach, several.ok = TRUE)
  approach <- unique(approach)
  assert_that(bootstrap == 0 || is.count(bootstrap))
  assert_that(is.flag(warnings))

  # Prepare .data for analysis
  d <- prep_data_dim(.data)

  # Warn about bootstrapping samples with less than 20 objects
  if (d$n_objects < 20 && bootstrap > 0 && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

}

# ICC for single score under Model 1A
# Model 1A applies when each object is rated by a different group of raters
calc_icc_1A_1 <- function(ratings) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model1(ratings, "A", n, s)

  # ICC estimate
  est <- v$object / (v$object + v$residual)

  # Create and label output vector
  out <- c(Object = v$object, Residual = v$residual, ICC = est)

  out
}

calc_icc_1A_k <- function(ratings, k = NULL) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model1(ratings, "B", n, s)

  if (is_null(k)) {k <- n$raters}

  # ICC estimate
  est <- v$object / (v$object + v$residual / k)

  # Create and label output vector
  out <- c(Object = v$object, Residual = v$residual, ICC = est)

  out
}

# ICC for single score under Model 1B
# Model 1B applies when each rater rates a different group of objects
calc_icc_1B_1 <- function(ratings) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model1(ratings, "B", n, s)

  # ICC estimate
  est <- v$rater / (v$rater + v$residual)

  # Create and label output vector
  out <- c(Rater = v$rater, Residual = v$residual, ICC = est)

  out
}

# ICC for single score under Model 2
# Model 2 applies when both raters and objects are random
calc_icc_2_1 <- function(ratings) {

  n <- icc_counts(ratings)
  s <- icc_sums(ratings, n)
  v <- icc_model2(ratings, n, s)

  # ICC estimate
  est <- v$object / (v$object + v$rater + v$interaction + v$residual)

  # Create and label output vector
  out <- c(Object = v$object, Rater = v$rater, Interaction = v$interaction, Residual = v$residual)

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

  out <- list()

  # Sum of all ratings per object
  ysum_o <- rowSums(ratings, na.rm = TRUE)

  # Sum of all ratings per rater
  ysum_r <- colSums(ratings, na.rm = TRUE)

  # Total sum of ratings
  out$T_y <- sum(ratings, na.rm = TRUE)

  # Total sum of squared ratings
  out$T_ysq <- sum(ratings^2, na.rm = TRUE)

  # Sum of all object's averaged squared ratings
  out$T_objects <- sum(ysum_o^2 / n$trials_o, na.rm = TRUE)

  # Sum of all raters' average squared ratings
  out$T_raters <- sum(ysum_r^2 / n$trials_r, na.rm = TRUE)

  out$T_interaction <- sum(ratings^2 / n$trials_or, na.rm = TRUE)

  # Expected number of trials per object
  out$expt_object <- sum(n$trials_or^2 / n$trials_r)

  # Expected number of trails per rater
  out$expt_rater <- sum(n$trials_or^2 / n$trials_o)

  out

}

#
icc_model1 <- function(ratings, type = c("A", "B"), n = NULL, s = NULL) {

  type <- match.arg(type)
  if (is_null(n)) {n <- icc_counts(ratings)}
  if (is_null(s)) {s <- icc_sums(ratings, n)}

  v <- list()

  if (type == "A") {
    v$residual <- (s$T_ysq - s$T_objects) / (n$trials - n$objects)
    v$object <- (s$T_objects - (s$T_y^2 / n$trials) - v$residual * (n$objects - 1)) / (n$trials - s$expt_object)
  } else if (type == "B") {
    v$residual <- (s$T_ysq - s$T_raters) / (n$trials - n$raters)
    v$rater <- (s$T_raters - (s$T_y^2 / n$trials) - v$residual * (n$raters - 1)) / (n$trials - s$expt_rater)
  }

  v

}

