#' Summarize data for plotting
#'
#' The function summarizes data for plotting.
#'
#' @param data the datatable containing the data to be plotted
#'
#' @return The function returns the summarized datatable.
#' @export
summarise_plot_data <- function(data) {
  data %>%
    group_by(survey_name, condition) %>%
    summarise(mean = mean(rate, na.rm = T),
              median = median(rate, na.rm = T),
              sd = sd(rate, na.rm = T),
              lower = mean - sd,
              upper = mean + sd)
}
