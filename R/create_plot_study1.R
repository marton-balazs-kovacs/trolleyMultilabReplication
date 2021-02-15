#' Create a plot for study1
#'
#' The function creates a plot for the
#' trolley problem paper specifically from already prepared
#' data.
#'
#' @param data the datatable to be plotted
#' @param data_sum summarised plot data
#'
#' @return The function returns a ggplot2 list object.
#' @export
create_plot_study1 <- function(data, data_sum) {
  data %>%
    ggplot() +
    aes(
      x = condition,
      y = rate,
      fill = condition) +
    # geom_flat_violin(
    #   position = position_nudge(x = 0, y = 0),
    #   alpha = .8) +
    stat_summary(
      fun = mean,
      geom = "line",
      aes(group = 1),
      position = position_nudge(x = 0.1),
      size = 1) +
    geom_line(
      data = data_sum,
      aes(x = condition, y = mean),
      position = position_nudge(x = 0.1),
      size = 2.5) +
    geom_point(
      data = data_sum,
      aes(x = condition, y = mean),
      position = position_nudge(x = 0.1),
      size = 2.5) +
    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_fill_viridis_d() +
    facet_wrap(facets = vars(survey_name)) +
    labs(y = "To what extent is this action morally acceptable?") +
    expand_limits(y = c(1,9)) +
    scale_y_continuous(breaks = 1:9) +
    theme_trolley()
}
