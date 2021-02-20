#' Create a plot to visualize the effect of individualism/collectivism scale on moral dilemmas
#'
#' This function ...
#'
#' @param data datatable to be plotted
#'
#' @return The function returns a ggplot2 list object.
#' @export
create_plot_ind_col <- function(data) {
  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = variable,
      y = b,
      width = 0.7) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      colour = "Black",
      width = 1.2) +
    ggplot2::geom_errorbar(
      aes(
        ymin = lower,
        ymax = higher),
      position = position_dodge(width = 0.7),
      colour = "Black",
      width = 0.2) +
    ggplot2::labs(y = "Beta effect size") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_y_continuous(breaks = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4), limits=c(-0.4,0.4)) +
    theme_trolley()
}
