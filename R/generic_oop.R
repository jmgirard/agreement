# Create a new S3 class from a list
new_s3_lst <- function(x, ..., class) {
  stopifnot(is.list(x))
  stopifnot(is.character(class))
  structure(x, ..., class = class)
}

# Create a new S3 class from a scalar
new_s3_scalar <- function(..., class) {
  new_s3_lst(list(...), class = class)
}
