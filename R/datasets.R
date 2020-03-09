#' Example dataset with unordered categorical ratings in wide format
#'
#' A tibble in wide format with variables denoting each rater. Example data with
#' 12 objects rated by 4 raters into 5 unordered categories denoted by the
#' letters \code{a}, \code{b}, \code{c}, \code{d}, and \code{e}. Includes some
#' missing data as \code{NA}. Adapted from Gwet (2014) Table 2.15 on page 55 as
#' part of Example 2.4.
#'
#' @seealso \code{\link{unordered}}
#'
#' @references Gwet, K. L. (2014). Handbook of inter-rater reliability: The
#'   definitive guide to measuring the extent of agreement among raters (4th
#'   ed.). Gaithersburg, MD: Advanced Analytics.
"unordered_wide"

#' Example dataset with unordered categorical ratings in tall format
#'
#' A tibble in tall format with variables denoting each \code{Object},
#' \code{Rater}, and \code{Score}. Example data with 12 objects rated by 4
#' raters into 5 unordered categories denoted by the letters \code{a}, \code{b},
#' \code{c}, \code{d}, and \code{e}. Includes some missing data as \code{NA}.
#' Adapted from Gwet (2014) Table 2.15 on page 55 as part of Example 2.4.
#'
#' @seealso \code{\link{unordered_wide}}
#'
#' @references Gwet, K. L. (2014). Handbook of inter-rater reliability: The
#'   definitive guide to measuring the extent of agreement among raters (4th
#'   ed.). Gaithersburg, MD: Advanced Analytics.
#'
"unordered"

#' Example dataset with ordered categorical ratings in wide format
#'
#' A tibble in wide format with variables denoting each rater. Example data with
#' 20 objects rated by 5 raters into 4 categories denoted by the numbers
#' \code{0}, \code{1}, \code{2}, and \code{3}. Includes some missing data as
#' (\code{NA}). Adapted from Gwet (2014) Table 4.9 on page 125 as part of
#' Example 4.4.
#'
#' @seealso \code{\link{ordered}}
#'
#' @references Gwet, K. L. (2014). Handbook of inter-rater reliability: The
#'   definitive guide to measuring the extent of agreement among raters (4th
#'   ed.). Gaithersburg, MD: Advanced Analytics.
"ordered_wide"

#' Example dataset with ordered categorical ratings from Gwet
#'
#' A tibble in tall format with variables denoting each \code{Object},
#' \code{Rater}, and \code{Score}. Example data with 20 objects rated by 5
#' raters into 4 categories denoted by the numbers \code{0}, \code{1}, \code{2},
#' and \code{3}. Includes some missing data as (\code{NA}). Adapted from Gwet
#' (2014) Table 4.9 on page 125 as part of Example 4.4.
#'
#' @seealso \code{\link{ordered_wide}}
#'
#' @references Gwet, K. L. (2014). Handbook of inter-rater reliability: The
#'   definitive guide to measuring the extent of agreement among raters (4th
#'   ed.). Gaithersburg, MD: Advanced Analytics.
"ordered"

#' Example dataset with interval categorical ratings from Gwet
#'
#' An object-by-rater tibble containing category assignments. Example data with
#' 16 objects rated by 4 raters into 5 interval categories denoted by the
#' numbers \code{0.5}, \code{1.0}, \code{1.5}, \code{2.0}, and \code{2.5}.
#' Includes some missing data as (\code{NA}). Adapted from Gwet (2014) Table 3.9
#' on page 90 as part of Example 3.3.
#'
#' @references Gwet, K. L. (2014). Handbook of inter-rater reliability: The
#'   definitive guide to measuring the extent of agreement among raters (4th
#'   ed.). Gaithersburg, MD: Advanced Analytics.
"interval"
