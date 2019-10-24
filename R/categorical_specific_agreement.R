#' @export
cat_specific <- function(.data,
                          categories = NULL,
                          bootstrap = 2000,
                          warnings = TRUE) {

  # Validate inputs
  assert_that(is.data.frame(.data) || is.matrix(.data))
  assert_that(is_null(categories) || is_vector(categories))
  assert_that(bootstrap == 0 || is.count(bootstrap))
  assert_that(is.flag(warnings))

  # Prepare data for analysis
  d <- prep_data(.data, categories, "identity", warnings)

  # Warn about samples with less than 20 objects
  if (bootstrap > 0 && d$n_objects < 20 && warnings == TRUE) {
    warning("With a small number of objects, bootstrap confidence intervals may not be stable.")
  }

  # Warn about bootstrapping with fewer than 1000 resamples
  if (bootstrap > 0 && bootstrap < 1000 && warnings == TRUE) {
    warning("To get stable confidence intervals, consider using more bootstrap resamples.")
  }

  # Create function to perform bootstrapping
  boot_function <- function(codes, index, categories, weight_matrix) {
    resample <- codes[index, , drop = FALSE]
    calc_sa(resample, categories, weight_matrix)
  }

  # Calculate the bootstrap results
  boot_results <-
    boot::boot(
      data = d$codes,
      statistic = boot_function,
      R = bootstrap,
      categories = d$categories,
      weight_matrix = d$weight_matrix
    )

  # Construct spa class output object
  out <- new_spa(
    observed = boot_results$t0,
    boot_results = boot_results,
    details = d,
    call = match.call()
  )

  out
}

# Worker function to calculate specific agreement
calc_sa <- function(codes, categories, weight_matrix) {

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories = categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc, na.rm = TRUE)

  # How much agreement was observed for each object-category combination?
  obs_oc <- r_oc * (t(weight_matrix %*% t(r_oc)) - 1)

  # How much agreement was observed for each category across all objects?
  obs_c <- colSums(obs_oc, na.rm = TRUE)

  # How much agreement was possible for each object-category combination?
  max_oc <- r_oc * (r_o - 1)

  # How much agreement was possible for each category across all objects?
  max_c <- colSums(max_oc, na.rm = TRUE)

  # What was the percent observed agreement for each category across all objects?
  poa_c <- obs_c / max_c

  # Replace non-finite results with missing values
  poa_c[!is.finite(poa_c)] <- NA

  # Label the output vector with category names
  names(poa_c) <- categories

  poa_c
}

