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
