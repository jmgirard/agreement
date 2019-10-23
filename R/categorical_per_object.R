#' @export
cat_per_object <- function(.data,
                           categories = NULL,
                           weighting = c("identity", "linear", "quadratic"),
                           warnings = TRUE) {

  # Validate inputs
  assert_that(is.data.frame(.data) || is.matrix(.data))
  weighting <- match.arg(weighting)

  # Prepare .data for analysis
  d <- prep_data(.data, categories, weighting, warnings)

  # Calculate weighted agreement per object
  obs_o <- calc_agreement_object(d$codes, d$categories, d$weight_matrix)

  # Create output tibble
  out <- tibble(Object = names(obs_o), Weighting = weighting, Agreement = obs_o)

  out
}

calc_agreement_object <- function(codes, categories, weight_matrix) {

  # How many raters assigned each object to each category?
  r_oc <- raters_obj_cat(codes, categories)

  # How many raters assigned each object to any category?
  r_o <- rowSums(r_oc)

  # How much agreement was observed for each object-category combination?
  obs_oc <- r_oc * (t(weight_matrix %*% t(r_oc)) - 1)

  # How much agreement was observed for each object across all categories?
  obs_o <- rowSums(obs_oc)

  # How much agreement was maximally possible for each object?
  max_o <- r_o * (r_o - 1)

  # What was the percent observed agreement for each object?
  poa_o <- obs_o[r_o >= 2] / max_o[r_o >= 2]

  poa_o
}
