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
    stat_summary(
      fun = mean,
      geom = "bar",
      position = "dodge",
      colour = "Black",
      width = 0.8) +
    stat_summary(
      fun.data = mean_cl_boot,
      geom = "errorbar",
      position = position_dodge(width = 0.7),
      colour = "Black",
      width = 0.2) +
    scale_fill_viridis_d() +
    facet_wrap(facets = vars(survey_name)) +
    labs(y = "To what extent is this action morally acceptable?") +
    coord_cartesian(ylim = c(1, 9)) +
    scale_y_continuous(breaks = 1:9, expand=c(0,0)) +
    theme_trolley()+
    theme(legend.position = "none")
}
