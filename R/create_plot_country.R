#' Create country and effect size scatter plot
#'
#' This function creates a ....
#'
#' @param data the prepared datatable for plotting
#'
#' @return The function returns a ggplot2 list object.
#' @export
create_plot_country <- function(data) {
  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = cohend,
      y = Collectivism,
      label = country) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = N,
        color = Region)) +
    ggplot2::guides(size = FALSE) +
    ggplot2::geom_text(hjust = -0.2, size = 3) +
    ggplot2::geom_smooth(
      method = "lm",
      mapping = ggplot2::aes(weight = N),
      color = "blue",
      show.legend = FALSE,
      se = F) +
    ggplot2::xlab("Effect size") +
    ggplot2::scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15), limits = c(0, 0.15)) +
    ggplot2::scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8))+
    theme_trolley()
}
