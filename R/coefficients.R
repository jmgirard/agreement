#' Calculate all chance-adjusted indexes of categorical agreement
#'
#' This function calculates
#'
#' @param .data Required.
#' @param categories Optional.
#' @param weighting Optional.
#'
#' @return A 5x4 tibble where each row corresponds to an index of categorical
#'   agreement (i.e., S, gamma, kappa, pi, and) and the columns are the
#'   following: \item{Approach}{The name of the coefficient}
#'   \item{Observed}{Percent of agreement observed in the data (given the
#'   coefficients' assumptions)} \item{Expected}{Percent of agreement expected
#'   by chance (given the coefficients' assumptions)} \item{Adjusted}{The
#'   estimated chance-adjusted agreement (i.e., the actual coefficient values)}
#' @export
#'
calc_all <- function(.data,
                     categories = NULL,
                     weighting = c("identity", "linear", "quadratic")) {

  weighting <- match.args(weighting)

  bind_rows(
    calc_s(.data, categories, weighting),
    calc_gamma(.data, categories, weighting),
    calc_kappa(.data, categories, weighting),
    calc_pi(.data, categories, weighting),
    calc_alpha(.data, categories, weighting)
  )

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
#'
#' @return A 1x4 tibble with the following columns: \item{Approach}{The name of
#'   the coefficient} \item{Observed}{Percent of agreement observed in the data}
#'   \item{Expected}{Percent of agreement expected by chance (given the
#'   coefficient's assumptions)} \item{Adjusted}{The estimated chance-adjusted
#'   agreement (i.e., the S score itself)}
#'
#' @references \url{https://doi.org/10/brfm77}
#' @export
#'
calc_s <- function(.data,
                   categories = NULL,
                   weighting = c("identity", "linear", "quadratic")) {

  weighting <- match.args(weighting)

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_raw(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_s(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  tibble::tibble(
    Approach = "S",
    Observed = poa,
    Expected = pea,
    Adjusted = cai
  )

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
#'
#' @return A 1x4 tibble with the following columns: \item{Approach}{The name of
#'   the coefficient} \item{Observed}{Percent of agreement observed in the data}
#'   \item{Expected}{Percent of agreement expected by chance (given the
#'   coefficient's assumptions)} \item{Adjusted}{The estimated chance-adjusted
#'   agreement (i.e., the alpha coefficient itself)}
#'
#' @references \url{https://doi.org/10/frj36r}
#' @export
#'
calc_gamma <- function(.data,
                       categories = NULL,
                       weighting = c("identity", "linear", "quadratic")) {

  weighting <- match.args(weighting)

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_raw(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_gamma(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  tibble::tibble(
    Approach = "Gamma",
    Observed = poa,
    Expected = pea,
    Adjusted = cai
  )

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
#'
#' @return A 1x4 tibble with the following columns: \item{Approach}{The name of
#'   the coefficient} \item{Observed}{Percent of agreement observed in the data}
#'   \item{Expected}{Percent of agreement expected by chance (given the
#'   coefficient's assumptions)} \item{Adjusted}{The estimated chance-adjusted
#'   agreement (i.e., the kappa coefficient itself)}
#'
#' @references \url{https://doi.org/10/dghsrr}
#' @references \url{https://doi.org/10/dpbw5f}
#' @export
#'
calc_kappa <- function(.data,
                       categories = NULL,
                       weighting = c("identity", "linear", "quadratic")) {

  weighting <- match.args(weighting)

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_raw(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_kappa(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  tibble::tibble(
    Approach = "Kappa",
    Observed = poa,
    Expected = pea,
    Adjusted = cai
  )

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
#'
#' @return A 1x4 tibble with the following columns: \item{Approach}{The name of
#'   the coefficient} \item{Observed}{Percent of agreement observed in the data}
#'   \item{Expected}{Percent of agreement expected by chance (given the
#'   coefficient's assumptions)} \item{Adjusted}{The estimated chance-adjusted
#'   agreement (i.e., the pi coefficient itself)}
#'
#' @references \url{https://doi.org/10/bzw9xp}
#' @export
#'
calc_pi <- function(.data,
                    categories = NULL,
                    weighting = c("identity", "linear", "quadratic")) {

  weighting <- match.args(weighting)

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_raw(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_pi(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  tibble::tibble(
    Approach = "Pi",
    Observed = poa,
    Expected = pea,
    Adjusted = cai
  )

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
#'
#' @return A 1x4 tibble with the following columns: \item{Approach}{The name of
#'   the coefficient} \item{Observed}{Percent of agreement observed in the data}
#'   \item{Expected}{Percent of agreement expected by chance (given the
#'   coefficient's assumptions)} \item{Adjusted}{The estimated chance-adjusted
#'   agreement (i.e., the alpha coefficient itself)}
#'
#' @references \url{https://doi.org/10/b6v5kt}
#' @export
#'
calc_alpha <- function(.data,
                       categories = NULL,
                       weighting = c("identity", "linear", "quadratic")) {

  weighting <- match.args(weighting)

  d <- prep_data(.data, categories, weighting)

  # Calculate percent observed agreement
  poa <- agree_alpha(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate percent expected agreement
  pea <- chance_alpha(d$codes, d$cat_possible, d$weight_matrix)

  # Calculate chance-adjusted index
  cai <- adjust_chance(poa, pea)

  # Return result
  tibble::tibble(
    Approach = "Alpha",
    Observed = poa,
    Expected = pea,
    Adjusted = cai
  )

}

