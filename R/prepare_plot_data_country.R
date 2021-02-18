#' Prepare data for the cultural distance and effect size comparison
#'
#' This function ...
#'
#' @param data datatable to be plotted
#' @param study_name name of the study, either study1a, study1b, study2a, or study2b
#'
#' @return The function returns a datatable ready to be plotted.
#' @export
prepare_plot_data_country <- function(data, study_name) {
  response_cols <-
    switch(study_name,
           "study1a" = c("trolley_1_rate", "trolley_2_rate"),
           "study1b" = c("speedboat_1_rate", "speedboat_2_rate"),
           "study2a" = c("trolley_3_rate", "trolley_4_rate", "trolley_5_rate", "trolley_6_rate"),
           "study2b" = c("speedboat_3_rate", "speedboat_4_rate", "speedboat_5_rate", "speedboat_6_rate"))

  data%>%
    select(country3, Region, response_cols) %>%
    pivot_longer(matches("_rate"), names_to = "condition", names_pattern = "(.*)_rate", values_to = "rate", values_drop_na = TRUE) %>%
    mutate(personal_force = if_else(str_detect(condition, "4|6"), 1, 0),
           intention = if_else(str_detect(condition, "4|5"), 1, 0)) %>%
    nest(data = c(condition, rate, personal_force, intention)) %>%
    left_join(., select(cultural_distance, Collectivism, country3), by = "country3") %>%
    mutate(N = map_int(data, nrow)) %>%
    mutate(cohend =  if_else(N >= 10,
                           map_dbl(data,
                                   possibly(
                                     ~ effectsize::eta_squared(aov(rate ~ intention : personal_force, data = .x))$Eta2,
                                     NA_real_)),
                           NA_real_)) %>%
    rename(country = country3) %>%
    select(-data) %>%
    drop_na(Collectivism, cohend)
}
