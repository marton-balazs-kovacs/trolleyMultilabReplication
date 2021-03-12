#' Create a plot for study1
#'
#' The function creates a plot for the
#' trolley problem paper specifically from already prepared
#' data.
#'
#' @param data the datatable to be plotted
#'
#' @return The function returns a ggplot2 list object.
#' @export
#'

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

create_plot_study1 <- function(data) {
  data %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = condition,
      y = rate,
      fill = condition,
      colour=condition) +
    geom_flat_violin(aes(y=rate, colour=condition),position = position_nudge(x = .2, y = 0), alpha = .8)+
    geom_point(aes(y=rate, colour=condition), position = position_jitter(width = .15), size = .25, alpha= 0.8)+
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      position = position_nudge(x = .3, y = 0),
      colour=c("White", "Black","White", "Black","White", "Black")) +
    ggplot2::stat_summary(
      fun.data = mean_cl_boot,
      geom = "errorbar",
      width = 0,
      position = position_nudge(x = .3, y = 0),
      colour=c("White", "Black","White", "Black","White", "Black"),
      show.legend=F) +
    scale_fill_viridis_d() +
    scale_colour_viridis_d() +
    ggplot2::facet_wrap(facets = vars(survey_name)) +
    ggplot2::labs(y = "To what extent is this action morally acceptable?") +
    ggplot2::coord_cartesian(ylim = c(1, 9)) +
    ggplot2::scale_y_continuous(breaks = 1:9, expand = c(0, 0)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 17),
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold", size = 10),
      axis.text.x = element_text(face = "bold", size = 7, colour = "Black"),
      axis.text.y = element_text(face = "bold", size = 12, colour = "Black"),
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      legend.position = "none")
}
