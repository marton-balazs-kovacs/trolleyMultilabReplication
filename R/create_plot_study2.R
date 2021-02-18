#' Create plot for study2
#'
#' The function creates plot for the
#' trolley problem paper specifically from already prepared
#' data.
#'
#' @param data the datatable to be plotted
#'
#' @return The function returns a ggplot2 list object.
#' @export
create_plot_study2 <- function(data) {
  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = intention,
      y = rate,
      fill = personal_force,
      width = 0.7) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "bar",
      position = "dodge",
      colour = "Black",
      width = 1.2) +
    ggplot2::stat_summary(
      fun.data = mean_cl_boot,
      geom = "errorbar",
      position = position_dodge(width = 0.7),
      colour = "Black",
      width = 0.2) +
    scale_fill_viridis_d() +
    ggplot2::facet_wrap(facets = vars(survey_name)) +
    ggplot2::labs(y = "To what extent is this action morally acceptable?") +
    ggplot2::coord_cartesian(ylim = c(1, 9)) +
    ggplot2::scale_y_continuous(breaks = 1:9, expand=c(0,0)) +
    theme_trolley()+
    ggplot2::theme(legend.position = c(0.8, 0.9))
}
