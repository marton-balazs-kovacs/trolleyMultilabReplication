#' Calculate interaction stats for study2 interaction terms
#'
#' This is the description.
#'
#' @param df a dataframe, either "study2a" or "study2b"
#'
#' @return The function returns the Bayesian and frequentist
#'   stats for study2 3rd order interactions.
#' @export
calculate_interaction_stats <- function(df = NULL){

  # Create a tibble that contains the labels for the cultural variables
  cultural_vars <-
    tibble::tibble(var = c("Collectivism", "ver_ind", "hor_ind", "ver_col", "hor_col"),
           variable = c("Country-level collectivism", "Vertical Individualism", "Horizontal Individualism", "Vertical Collectivism", "Horizontal Collectivism"))

  # Create a nested tibble that can be mapped through
  df %>%
    dplyr::select(country3, trolley_3_rate, trolley_4_rate,
           trolley_5_rate, trolley_6_rate,
           all_of(cultural_vars$var)) %>%
    # Collapse conditions
    tidyr::pivot_longer(cols = c(trolley_3_rate, trolley_4_rate,
                          trolley_5_rate, trolley_6_rate),
                 names_to = "condition",
                 values_to = "rate",
                 values_drop_na = TRUE) %>%
    # Creating predictors based on the condition
    dplyr::mutate(personal_force = dplyr::if_else(str_detect(condition, "3|5"), 0, 1),
           intention = dplyr::if_else(str_detect(condition, "4|5"), 1, 0),
           # What is this??? lmBF doesn't run without it?!?!?
           country0 = paste0("0", country3)) %>%
    tidyr::drop_na() %>%
    # Create separate nested datasets to all cultural variables
    tidyr::pivot_longer(cols = cultural_vars$var, names_to = "var") %>%
    dplyr::group_by(var) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    # Map through the datasets and create stat models using a specific cultural variable
    dplyr::mutate(datt = purrr::map2(data, var,
                       ~BayesFactor::lmBF(rate ~ personal_force*intention*value + country0,
                             whichRandom = "country0",
                             data = as.data.frame(.x),
                             # Set prior dynamically, based on the interaction with a specific variable
                             rscaleCont = set_names(0.19, str_glue("personal_force:intention:{.y}")))),
           datt2 = purrr::map(data,
                       ~BayesFactor::lmBF(rate ~ personal_force + intention + value +
                               personal_force:value +
                               personal_force:intention + intention:value +
                               country0,
                             whichRandom = "country0",
                             data = as.data.frame(.x))),
           datt3 = purrr::map2(datt, datt2, ~BayesFactor::recompute((.x/.y), iterations = 50000) %>%
                          as_tibble()),
           frequentist = purrr::map(data,
                             ~lmerTest::lmer(rate ~ personal_force*intention*value + (1|country3),
                                   data = .x) %>%
                               broom.mixed::tidy(conf.int = TRUE))) %>%
    #
    dplyr::left_join(cultural_vars, by = "var") %>%
    dplyr::transmute(variable,
              BF = purrr::map_dbl(datt3, ~dplyr::slice(.x) %>%
                                    dplyr::pull(bf) %>%
                             round(4)),
              b = purrr::map_dbl(frequentist,
                          ~dplyr::filter(.x, term == "personal_force:intention:value") %>%
                            dplyr::pull(estimate) %>%
                            round(4)),
              lower = purrr::map_dbl(frequentist,
                              ~dplyr::filter(.x, term == "personal_force:intention:value") %>%
                                dplyr::pull(conf.low) %>%
                                round(4)),
              higher = purrr::map_dbl(frequentist,
                               ~dplyr::filter(.x, term == "personal_force:intention:value") %>%
                                 dplyr::pull(conf.high) %>%
                                 round(4)),
              p = purrr::map_dbl(frequentist,
                          ~dplyr::filter(.x, term == "personal_force:intention:value") %>%
                            dplyr::pull(p.value) %>%
                            round(4))
    )

}
