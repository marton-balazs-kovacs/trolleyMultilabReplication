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
    geom_point(aes(size =N )) +
    geom_text(hjust = -0.2, size = 3) +
    geom_smooth(
      method = "lm",
      mapping = aes(weight = N),
      color = "blue",
      show.legend = FALSE,
      se = F) +
    xlab("Effect size") +
    theme_trolley()
}
