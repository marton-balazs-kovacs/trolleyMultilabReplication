#' Prepare datatable for plotting for study2
#'
#' Prepares datatable for plotting.
#'
#' @param data the datatable to be plotted
#' @param study_type the type of the study either "trolley" or "speedboat"
#'
#' @return the function returns a modified datatable
#' ready to be passed to the plotting function.
#' @export
prepare_plot_data_study2 <- function(data, study_type) {
  response_cols <-
    switch(study_type,
           "trolley" = c("trolley_3_rate", "trolley_4_rate", "trolley_5_rate", "trolley_6_rate"),
           "speedboat" = c("speedboat_3_rate", "speedboat_4_rate", "speedboat_5_rate", "speedboat_6_rate"))

  data %>%
    select(survey_name, response_cols) %>%
    mutate(survey_name = factor(survey_name)) %>%
    pivot_longer(
      -survey_name,
      names_to = "condition",
      values_to = "rate") %>%
    mutate(personal_force = if_else(str_detect(condition, "4|6"), "Personal Force", "No personal force"),
           intention = if_else(str_detect(condition, "3|6"), "No Intention", "Intention"),
           survey_name = str_remove(survey_name, "PSA006_")) %>%
    drop_na(rate)
}

