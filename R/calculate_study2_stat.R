#' Calculates the statistics for study 2
#'
#' description...
#'
#' @param data: data frame or tibble that contains Region and specified variables
#' @param vars: the variables to be collapsed
#' @param label: what data label should be attached to the output
#'
#' @return a tibble that contains the selected statistics
#'   in a format that can be printed
#' @export
calculate_study2_stat <- function(data = NULL,
                                  vars = NULL,
                                  label = NULL){

  data %>%
    as_tibble() %>%
    dplyr::select(Region, {{vars}}) %>%
    dplyr::group_by(Region) %>%
    nest() %>%
    dplyr::arrange(Region) %>%
    dplyr::mutate(data_long = purrr::map(data,
                           ~pivot_longer(.x,
                                         cols = everything(),
                                         names_to = "condition",
                                         values_to = "rate",
                                         values_drop_na = TRUE) %>%
                             dplyr::mutate(personal_force = factor(if_else(str_detect(condition, "3|5")), 1, 0),
                                    intention = factor(if_else(str_detect(condition, "4|5"), 1, 0))) %>%
                             as.data.frame()),
           bmod_1 = purrr::map(data_long,
                        ~BayesFactor::lmBF(rate ~ personal_force * intention,
                              data = .x,
                              rscaleFixed = rscaleFixed)),

           bmod_2 = purrr::map(data_long,
                        ~BayesFactor::lmBF(rate ~ personal_force + intention,
                              data = .x,
                              rscaleFixed = rscaleFixed)),
           bmod = purrr::map2(bmod_1, bmod_2,
                       ~BayesFactor::recompute(.x / .y, iterations = 50000) %>%
                         as_tibble()),
           fmod = purrr::map(data_long,
                      ~aov(rate ~ personal_force * intention, data=.x) %>%
                        broom::tidy())) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Exclusion = label,
      Cluster = Region,
      BF = purrr::map_chr(bmod,
                   ~slice(.x, 1) %>%
                     pull(bf) %>%
                     scales::scientific()),
      RR = NA_character_,
      `F` = purrr::map_dbl(fmod,
                    ~filter(.x, term == "personal_force:intention") %>%
                      pull(statistic) %>%
                      round(3)),
      df = purrr::map_chr(fmod,
                   ~filter(.x, term == "Residuals") %>%
                     pull(df) %>%
                     paste0("1, ", .)),
      p = purrr::map_chr(fmod,
                  ~filter(.x, term == "personal_force:intention") %>%
                    pull(p.value) %>%
                    scales::scientific()),
      `Eta squared` = purrr::map_dbl(data_long,
                              ~aov(rate ~ personal_force*intention,
                                   data=.x) %>%
                                effectsize::eta_squared() %>%
                                as_tibble() %>%
                                filter(Parameter == "personal_force:intention") %>%
                                pull(Eta2_partial) %>%
                                round(3)),
      `Raw effect` = purrr::map_dbl(data_long,
                             ~group_by(.x, personal_force, intention) %>%
                               summarise(avg_rate = mean(rate, na.rm = TRUE),
                                         .groups = "drop") %>%
                               pivot_wider(names_from = c(personal_force, intention),
                                           values_from = avg_rate) %>%
                               mutate(raw_diff = (`0_0`-`1_0`) - (`0_1` - `1_1`)) %>%
                               pull(raw_diff) %>%
                               round(2))
    )
}

