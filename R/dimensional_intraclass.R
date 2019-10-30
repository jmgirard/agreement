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
  d <- prep_data_dim(.data, categories, weighting, warnings)

  # Warn about bootstrapping samples with less than 20 objects
  if (d$n_objects < 20 && bootstrap > 0 && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

}

# ICC for single score under Model 1A
# Model 1A applies when each object is rated by a different group of raters
calc_icc_1A_1 <- function(ratings) {

  n_objects <- nrow(ratings)

  m_ij <- is.finite(ratings) #TODO: calc from array
  m_i <- rowSums(m_ij)
  m_j <- colSums(m_ij)
  M <- sum(m_ij)
  msq_ij <- m_ij^2

  y_i <- rowSums(ratings, na.rm = TRUE)
  y_j <- colSums(ratings, na.rm = TRUE)

  ysq_i <- y_i^2
  ysq_m_i <- ysq_i / m_i
  y_2i <- rowSums(ratings^2, na.rm = TRUE)

  T_y <- sum(ratings, na.rm = TRUE)
  T_2o <- sum(ysq_m_i, na.rm = TRUE)
  T_2y <- sum(y_2i, na.rm = TRUE)

  k_0 <- sum(msq_ij / m_j)

  var_e <- (T_2y - T_2o) / (M - n_objects)
  var_o <- (T_2o - (T_y^2 / M) - (n_objects - 1) * var_e) / (M - k_0)

  statistic <- var_o / (var_o + var_e)

  # Create and label output vector
  out <- c(var_o = var_o, var_e = var_e, icc = statistic)

  out
}

#TODO: Figure out how to adapt this to ICC_1A_k

# ICC for single score under Model 1B
# Model 1B applies when each rater rates a different group of objects
calc_icc_1B_1 <- function(ratings) {

  n_objects <- nrow(ratings)
  n_raters <- ncol(ratings)

  m_ij <- is.finite(ratings) #TODO: calc from array
  m_i <- rowSums(m_ij)
  m_j <- colSums(m_ij)
  M <- sum(m_ij)
  msq_ij <- m_ij^2

  y_i <- rowSums(ratings, na.rm = TRUE)
  y_j <- colSums(ratings, na.rm = TRUE)

  ysq_j <- y_j^2
  ysq_m_j <- ysq_j / m_j
  y_2j <- colSums(ratings^2, na.rm = TRUE)

  T_y <- sum(ratings, na.rm = TRUE)
  T_2r <- sum(ysq_m_j, na.rm = TRUE)
  T_2y <- sum(y_2j, na.rm = TRUE)

  k_1 <- sum(msq_ij / m_i)

  var_e <- (T_2y - T_2r) / (M - n_raters)
  var_r <- (T_2r - (T_y^2 / M) - (n_raters - 1) * var_e) / (M - k_1)

  statistic <- var_r / (var_r + var_e)

  # Create and label output vector
  out <- c(var_r = var_r, var_e = var_e, icc = statistic)

  out
}

#TODO: Figure out how to adapt this to ICC_1A_k





