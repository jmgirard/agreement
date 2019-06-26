#' Calculate all chance-adjusted indexes of categorical agreement
#'
#' This function calculates all chance-adjusted indexes of categorical agreement
#' (i.e., S, gamma, kappa, pi, and alpha) and then binds their results together
#' into a single tibble. All indexes are calculated using the same data,
#' categories and weighting scheme, so they are directly comparable.
#'
#' @param .data Required.
#' @param categories Optional.
#' @param weighting Optional.
#' @param format Optional.
#' @param digits Optional.
#'
#' @return A tibble containing the estimates of all chance-adjusted indexes of
#'   categorical agreement.
#' @export
#' @examples
#' fish <- data("fish")
#' calc_all(fish, categories = c(1, 2, 3), weighting = "identity")
calc_all <- function(.data,
                     categories = NULL,
                     weighting = c("identity", "linear", "quadratic"),
                     format = c("long", "wide"),
                     digits = 3) {

  weighting <- match.arg(weighting)
  format <- match.arg(format)
  assertthat::assert_that(rlang::is_scalar_integerish(digits))

  s <- calc_s(.data, categories, weighting, format)
  gamma <- calc_gamma(.data, categories, weighting, format)
  kappa <- calc_kappa(.data, categories, weighting, format)
  pi <- calc_pi(.data, categories, weighting, format)
  alpha <- calc_alpha(.data, categories, weighting, format)

  if (format == "long") {
    out <- dplyr::bind_rows(s, gamma, kappa, pi, alpha)
  } else if (format == "wide") {
    out <- dplyr::bind_cols(s, gamma, kappa, pi, alpha)
  }

  out <- dplyr::mutate_if(out, is.numeric, .funs = round, digits = digits)

  out

}
#' Calculate the S score using the generalized formula
#'
#' Bennett et al.'s S score is a chance-adjusted index of categorical agreement.
#' It estimates chance agreement using category-based assumptions. This function
#' calculates this coefficient using a generalized formula that can accommodate
#' multiple raters, multiple categories, and ordered or unordered categories
#' (through a weighting scheme).
#'
#' @param .data Required.
#' @param categories Optional.
#' @param weighting Optional.
#' @param format Optional.
#' @param digits Optional.
#'
#' @return A tibble containing the percent of agreement observed in the data,
#'   the percent of agreement expected by chance, and the chance-adjusted index
#'   of reliability (i.e., the coefficient itself). These numbers are all
#'   calculated under the assumptions of the coefficient.
#'
#' @references \url{https://doi.org/10/brfm77}
#' @export
#'
calc_s <- function(.data,
                   categories = NULL,
                   weighting = c("identity", "linear", "quadratic"),
                   format = c("long", "wide"),
                   digits = 3) {

  weighting <- match.arg(weighting)
  format <- match.arg(format)
  assertthat::assert_that(rlang::is_scalar_integerish(digits))

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_raw(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_s(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  if (format == "long") {
    out <-
      tibble::tibble(
        Approach = "S",
        Weights = weighting,
        Observed = poa,
        Expected = pea,
        Adjusted = cai
      )
  } else if (format == "wide") {
    out <-
      tibble::tibble(
        Obs_S = poa,
        Exp_S = pea,
        Adj_S = cai
      )
  }

  out <- dplyr::mutate_if(out, is.numeric, round, digits = digits)
  out
}

#' Calculate the gamma coefficient using the generalized formula
#'
#' Gwet's gamma coefficient is a chance-adjusted index of categorical agreement.
#' It estimates chance agreement using a hybrid model that combines category-
#' and distribution-based assumptions. This function calculates this coefficient
#' using a generalized formula that can accommodate multiple raters, multiple
#' categories, and ordered or unordered categories (through a weighting scheme).
#'
#' @param .data Required.
#' @param categories Optional.
#' @param weighting Optional.
#' @param format Optional.
#' @param digits Optional.
#'
#' @return A tibble containing the percent of agreement observed in the data,
#'   the percent of agreement expected by chance, and the chance-adjusted index
#'   of reliability (i.e., the coefficient itself). These numbers are all
#'   calculated under the assumptions of the coefficient.
#'
#' @references \url{https://doi.org/10/frj36r}
#' @export
#'
calc_gamma <- function(.data,
                       categories = NULL,
                       weighting = c("identity", "linear", "quadratic"),
                       format = c("long", "wide"),
                       digits = 3) {

  weighting <- match.arg(weighting)
  format <- match.arg(format)
  assertthat::assert_that(rlang::is_scalar_integerish(digits))

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_raw(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_gamma(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  if (format == "long") {
    out <-
      tibble::tibble(
        Approach = "gamma",
        Weights = weighting,
        Observed = poa,
        Expected = pea,
        Adjusted = cai
      )
  } else if (format == "wide") {
    out <-
      tibble::tibble(
        Obs_Gamma = poa,
        Exp_Gamma = pea,
        Adj_Gamma = cai
      )
  }

  out <- dplyr::mutate_if(out, is.numeric, round, digits = digits)
  out
}

#' Calculate the kappa coefficient using the generalized formula
#'
#' Cohen's kappa coefficient is a chance-adjusted index of categorical
#' agreement. It estimates chance agreement using distribution-based
#' assumptions. This function calculates this coefficient using a generalized
#' formula that can accommodate multiple raters, multiple categories, and
#' ordered or unordered categories (through a weighting scheme).
#'
#' @param .data Required.
#' @param categories Optional.
#' @param weighting Optional.
#' @param format Optional.
#' @param digits Optional.
#'
#' @return A tibble containing the percent of agreement observed in the data,
#'   the percent of agreement expected by chance, and the chance-adjusted index
#'   of reliability (i.e., the coefficient itself). These numbers are all
#'   calculated under the assumptions of the coefficient.
#'
#' @references \url{https://doi.org/10/dghsrr}
#' @references \url{https://doi.org/10/dpbw5f}
#' @export
#'
calc_kappa <- function(.data,
                       categories = NULL,
                       weighting = c("identity", "linear", "quadratic"),
                       format = c("long", "wide"),
                       digits = 3) {

  weighting <- match.arg(weighting)
  format <- match.arg(format)
  assertthat::assert_that(rlang::is_scalar_integerish(digits))

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_raw(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_kappa(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  if (format == "long") {
    out <-
      tibble::tibble(
        Approach = "kappa",
        Weights = weighting,
        Observed = poa,
        Expected = pea,
        Adjusted = cai
      )
  } else if (format == "wide") {
    out <-
      tibble::tibble(
        Obs_Kappa = poa,
        Exp_Kappa = pea,
        Adj_Kappa = cai
      )
  }

  out <- dplyr::mutate_if(out, is.numeric, round, digits = digits)
  out
}

#' Calculate the pi coefficient using the generalized formula
#'
#' Scott's pi coefficient is a chance-adjusted index of categorical agreement.
#' It estimates chance agreement using distribution-based assumptions. This
#' function calculates this coefficient using a generalized formula that can
#' accommodate multiple raters, multiple categories, and ordered or unordered
#' categories (through a weighting scheme).
#'
#' @param .data Required.
#' @param categories Optional.
#' @param weighting Optional.
#' @param format Optional.
#' @param digits Optional.
#'
#' @return A tibble containing the percent of agreement observed in the data,
#'   the percent of agreement expected by chance, and the chance-adjusted index
#'   of reliability (i.e., the coefficient itself). These numbers are all
#'   calculated under the assumptions of the coefficient.
#'
#' @references \url{https://doi.org/10/bzw9xp}
#' @export
#'
calc_pi <- function(.data,
                    categories = NULL,
                    weighting = c("identity", "linear", "quadratic"),
                    format = c("long", "wide"),
                    digits = 3) {

  weighting <- match.arg(weighting)
  format <- match.arg(format)
  assertthat::assert_that(rlang::is_scalar_integerish(digits))

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_raw(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_pi(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  if (format == "long") {
    out <-
      tibble::tibble(
        Approach = "pi",
        Weights = weighting,
        Observed = poa,
        Expected = pea,
        Adjusted = cai
      )
  } else if (format == "wide") {
    out <-
      tibble::tibble(
        Obs_Pi = poa,
        Exp_Pi = pea,
        Adj_Pi = cai
      )
  }
  out <- dplyr::mutate_if(out, is.numeric, round, digits = digits)
  out
}

#' Calculate the alpha coefficient using the generalized formula
#'
#' Krippendorff's alpha coefficient is a chance-adjusted index of categorical
#' agreement. It estimates chance agreement using distribution-based
#' assumptions. This function calculates this coefficient using a generalized
#' formula that can accommodate multiple raters, multiple categories, and
#' ordered or unordered categories (through a weighting scheme).
#'
#' @param .data Required.
#' @param categories Optional.
#' @param weighting Optional.
#' @param format Optional.
#' @param digits Optional.
#'
#' @return A tibble containing the percent of agreement observed in the data,
#'   the percent of agreement expected by chance, and the chance-adjusted index
#'   of reliability (i.e., the coefficient itself). These numbers are all
#'   calculated under the assumptions of the coefficient.
#'
#' @references \url{https://doi.org/10/b6v5kt}
#' @export
#'
calc_alpha <- function(.data,
                       categories = NULL,
                       weighting = c("identity", "linear", "quadratic"),
                       format = c("long", "wide"),
                       digits = 3) {

  weighting <- match.arg(weighting)
  format <- match.arg(format)
  assertthat::assert_that(rlang::is_scalar_integerish(digits))

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_alpha(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_alpha(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  if (format == "long") {
    out <-
      tibble::tibble(
        Approach = "alpha",
        Weights = weighting,
        Observed = poa,
        Expected = pea,
        Adjusted = cai
      )
  } else if (format == "wide") {
    out <-
      tibble::tibble(
        Obs_Alpha = poa,
        Exp_Alpha = pea,
        Adj_Alpha = cai
      )
  }
  out <- dplyr::mutate_if(out, is.numeric, round, digits = digits)
  out
}

