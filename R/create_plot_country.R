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
    ggplot() +
    aes(
      x = cohend,
      y = Collectivism,
      label = country) +
    geom_point(aes(size =N,color=Region )) +
    guides(size = FALSE)+
    geom_text(hjust = -0.2, size = 3) +
    geom_smooth(
      method = "lm",
      mapping = aes(weight = N),
      color = "blue",
      show.legend = FALSE,
      se = F) +
    xlab("Effect size") +
    scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15), limits=c(0,0.15)) +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8))+
    theme_trolley()
}
