chance_ckappa <- function(codes, categories, weights) {

  # Get basic counts
  n_objects <- nrow(codes)
  n_raters <- ncol(codes)
  n_categories <- length(categories)

  # Create objects (i.e., rater-by-category) matrix
  mat_objects <- matrix(0, nrow = n_raters, ncol = n_categories)
  for (k in seq_along(categories)) {
    mat_objects[, k] <- rowSums(codes == categories[[k]], na.rm = TRUE)
  }

}
