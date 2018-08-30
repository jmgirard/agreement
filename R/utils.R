finite <- function(x) {
  x[is.finite(x)]
}

get_unique <- function(x) {
  x %>%
    c() %>%
    unique() %>%
    finite() %>%
    sort()
}

validate_categories <- function(mat, categories) {

}

# Drop rows that contain only missing values
remove_uncoded <- function(mat) {
  mat[rowSums(is.na(mat)) != ncol(mat), ]
}

# Calculate chance-adjusted agreement
adjust_chance <- function(poa, pea) {
  (poa - pea) / (1 - pea)
}
