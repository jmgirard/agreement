#' Simulate ICC by Number of Raters
#'
#' @param object An object resulting from a call to \code{dim_icc()}.
#' @param min_r A nonnegative integer representing the minimum number of raters
#'   to be simulated. (default = 1)
#' @param max_r A nonnegative integer representing the maximum number of raters
#'   to be simulated.
#' @param plot A logical flag indicating whether to generate a simulation plot.
#'   (default = TRUE)
#' @param line_size A nonnegative number indicating the size of lines on the plot
#'   (default = 1).
#' @param point_size A nonnegative number indicating the size of points on the
#'   plot (default = 3).
#' @param xstep A nonnegative number indicating the desired spacing between
#'   breaks on the x-axis of the plot (default = 1).
#' @return A tibble containing the estimated inter-rater ICC by number of raters
#'   averaged.
#' @export
dim_icc_sim <- function(object, min_r = 1, max_r, plot = TRUE, line_size = 1, point_size = 3, xstep = 1) {
  # object must be icc class
  assert_that(is.flag(plot))
  assert_that(is.count(min_r), min_r >= 1)
  assert_that(is.count(max_r), max_r >= min_r)
  assert_that(is.number(xstep), xstep > 0)
  assert_that(is.number(line_size), line_size > 0)
  assert_that(is.number(point_size), point_size > 0)

  formulation <- object$formulation
  components <- as.list(object$boot_results$t0)
  names(components) <- tolower(names(components))

  out <- tibble::tibble(Raters = min_r:max_r)
  out <- mutate(
    out,
    Inter_ICC = purrr::map(
      Raters, ~calc_inter_icc(components, ., formulation$model, formulation$type)))
  out <- tidyr::unnest(out, cols = Inter_ICC)
  if (plot == TRUE) {
    p <- ggplot2::ggplot(out, aes(x = Raters, y = Inter_ICC)) +
      ggplot2::geom_hline(yintercept = 0.50, size = line_size, color = "grey", linetype = "dotted") +
      ggplot2::geom_hline(yintercept = 0.75, size = line_size, color = "grey", linetype = "dotted") +
      ggplot2::geom_hline(yintercept = 0.90, size = line_size, color = "grey", linetype = "dotted") +
      ggplot2::geom_line(size = line_size) +
      ggplot2::geom_point(size = point_size) +
      ggplot2::scale_x_continuous("Number of Raters Averaged",
                                  breaks = seq(min_r, max_r, by = xstep)) +
      ggplot2::scale_y_continuous("Inter-Rater ICC", breaks = seq(0, 1, by = 0.2)) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::theme_classic()
    print(p)
  }
  out
}
