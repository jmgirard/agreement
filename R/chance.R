chance_ckappa <- function(codes, categories, weights) {

  # Get basic counts
  n_o <- nrow(codes) # objects
  n_r <- ncol(codes) # raters
  n_c <- length(categories) # categories

  # Create object-counts in rater-by-category matrix
  mat_o_rc <- matrix(0, nrow = n_r, ncol = n_c)
  for (k in seq_along(categories)) {
    mat_o_rc[, k] <- colSums(codes == categories[[k]], na.rm = TRUE)
  }

  sum_o_r <- rowSums(rxc, na.rm = TRUE)
  mat_rc_max <- sum_o_r %*% matrix(1, nrow = 1, ncol = n_c)
  mat_ <- mat_objects / mat_counts
}
