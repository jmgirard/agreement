plot_agr <- function(df, Target) {
  ggplot(df, aes(x = fct_reorder(!!rlang::enquo(Target), -Adjusted))) +
    geom_col(aes(y = Observed, fill = "Observed Agreement")) +
    geom_col(aes(y = Adjusted, fill = "Chance-Adjusted Agreement")) +
    scale_y_continuous(NULL, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    scale_x_discrete(NULL) +
    scale_fill_manual(
      name = "",
      values = c("Chance-Adjusted Agreement" = "#2b8cbe",
                 "Observed Agreement" = "#a6bddb"),
      guide = guide_legend(reverse = TRUE)
    ) +
    theme(legend.position = "top")
}

plot_compare <- function(df) {
  ggplot(df, aes(x = Approach)) +
    geom_col(aes(y = Adjusted, fill = "Adjusted")) +
    geom_col(aes(y = Expected, fill = "Expected")) +
    scale_y_continuous(NULL, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    scale_x_discrete(NULL) +
    scale_fill_manual(
      name = "Approach",
      values = c("Adjusted" = "#2b8cbe",
                 "Expected" = "#a6bddb")
    ) +
    theme(legend.position = "top")
}
