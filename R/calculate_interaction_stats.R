#' Calculate interaction stats for study2 interaction terms
#'
#' This is the description.
#'
#' @param df a dataframe, either "study2a" or "study2b"
#' @param study_type the type of the study either "trolley" or "speedboat"
#'
#' @return The function returns the Bayesian and frequentist
#'   stats for study2 3rd order interactions.
#' @export
calculate_interaction_stats <- function(df = NULL, study_type) {
  # Get response columns
  response_cols <-
    switch(study_type,
           "trolley" = c("trolley_3_rate", "trolley_4_rate", "trolley_5_rate", "trolley_6_rate"),
           "speedboat" = c("speedboat_3_rate", "speedboat_4_rate", "speedboat_5_rate", "speedboat_6_rate"))

  # Create a tibble that contains the labels for the cultural variables
  cultural_vars <-
    tibble::tibble(var = c("Collectivism", "ver_ind", "hor_ind", "ver_col", "hor_col"),
           variable = c("Country-level collectivism", "V. Individualism", "H. Individualism", "V. Collectivism", "H. Collectivism"))

  # Create a nested tibble that can be mapped through
  df %>%
    dplyr::select(
      country3,
      all_of(response_cols),
      all_of(cultural_vars$var)) %>%
    # Collapse conditions
    tidyr::pivot_longer(
      cols = response_cols,
      names_to = "condition",
      values_to = "rate",
      values_drop_na = TRUE) %>%
    # Creating predictors based on the condition
    dplyr::mutate(
      personal_force = dplyr::if_else(str_detect(condition, "3|5"), 0, 1),
      intention = dplyr::if_else(str_detect(condition, "4|5"), 1, 0),
      personal_force2 = dplyr::if_else(str_detect(condition, "3|5"), -1, 1),
      intention2 = dplyr::if_else(str_detect(condition, "4|5"), 1, -1),
      personal_force = factor(personal_force),
      intention = factor(intention),
      # What is this??? lmBF doesn't run without it?!?!?
      country0 = factor(paste0("0", country3))) %>%
    # tidyr::drop_na() %>%
    # Create separate nested datasets to all cultural variables
    tidyr::pivot_longer(cols = cultural_vars$var, names_to = "var") %>%
    dplyr::group_by(var) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    # Map through the datasets and create stat models using a specific cultural variable
    dplyr::mutate(
      data = dplyr::if_else(var == "Collectivism", purrr::map(data, drop_na), data),
      datt = purrr::map2(data, var,
                         ~BayesFactor::lmBF(rate ~ personal_force*intention*value + country0,
                                            whichRandom = "country0",
                                            data = as.data.frame(.x),
                                            # Set prior dynamically, based on the interaction with a specific variable
                                            rscaleCont = purrr::set_names(0.19, stringr::str_glue("personal_force:intention:{.y}")))),
      datt2 = purrr::map(data,
                         ~BayesFactor::lmBF(rate ~ personal_force + intention + value + personal_force:value + personal_force:intention + intention:value + country0,
                                            whichRandom = "country0",
                                            data = as.data.frame(.x))),
      datt3 = purrr::map2(datt, datt2,
                          ~BayesFactor::recompute((.x/.y), iterations = 100000) %>%
                            as_tibble()),
      frequentist = purrr::map(data,
                               ~lmerTest::lmer(rate ~ personal_force2 * intention2 * value + (1|country3),
                                               data = .x) %>%
                                 broom.mixed::tidy(conf.int = TRUE)),
      n = map_dbl(data, nrow),
      hdi = purrr::map2(data, var,
                        ~ bayestestR::hdi(
                          BayesFactor::lmBF(rate ~ personal_force * intention * value + country0,
                                            whichRandom = "country0",
                                            data = as.data.frame(.x),
                                            # Set prior dynamically, based on the interaction with a specific variable
                                            rscaleCont = purrr::set_names(0.19, stringr::str_glue("personal_force:intention:{.y}"))),
                          c = 0.89))) %>%
    dplyr::left_join(cultural_vars, by = "var")

}
