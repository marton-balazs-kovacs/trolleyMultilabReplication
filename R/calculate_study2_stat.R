#' Calculates the statistics for study 2
#'
#' description...
#'
#' @param data data frame or tibble that contains Region and specified variables
#' @param vars the variables to be collapsed
#' @param label what data label should be attached to the output
#' @param rscaleFixed prior
#'
#' @return a tibble that contains the selected statistics
#'   in a format that can be printed
#' @export
calculate_study2_stat <- function(data = NULL,
                                  vars = NULL,
                                  label = NULL,
                                  rscaleFixed){

  data %>%
    tibble::as_tibble() %>%
    dplyr::select(Region, {{vars}}) %>%
    dplyr::group_by(Region) %>%
    tidyr::nest() %>%
    dplyr::arrange(Region) %>%
    dplyr::mutate(data_long = purrr::map(data,
                           ~tidyr::pivot_longer(.x,
                                         cols = everything(),
                                         names_to = "condition",
                                         values_to = "rate",
                                         values_drop_na = TRUE) %>%
                             dplyr::mutate(
                               personal_force = dplyr::if_else(stringr::str_detect(condition, "3|5"), 0, 1),
                               intention = dplyr::if_else(stringr::str_detect(condition, "4|5"), 1, 0),
                               personal_force = factor(personal_force),
                               intention = factor(intention)) %>%
                             as.data.frame()),
           bmod_1 = purrr::map(data_long,
                        ~BayesFactor::lmBF(rate ~ personal_force * intention,
                              data = .x,
                              rscaleFixed = rscaleFixed)),
           bmod_2 = purrr::map(data_long,
                        ~BayesFactor::lmBF(rate ~ personal_force + intention,
                              data = .x)),
           bmod = purrr::map2(bmod_1, bmod_2,
                       ~BayesFactor::recompute(.x / .y, iterations = 50000) %>%
                         tibble::as_tibble()),
           fmod = purrr::map(data_long,
                      ~aov(rate ~ personal_force * intention, data=.x) %>%
                        broom::tidy())) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Exclusion = label,
      Cluster = Region,
      BF = purrr::map_chr(bmod,
                   ~dplyr::slice(.x, 1) %>%
                     dplyr::pull(bf) %>%
                     scales::scientific()),
      RR = NA_character_,
      `F` = purrr::map_dbl(fmod,
                    ~dplyr::filter(.x, term == "personal_force:intention") %>%
                      dplyr::pull(statistic) %>%
                      round(3)),
      df = purrr::map_chr(fmod,
                   ~dplyr::filter(.x, term == "Residuals") %>%
                     dplyr::pull(df) %>%
                     paste0("1, ", .)),
      p = purrr::map_chr(fmod,
                  ~dplyr::filter(.x, term == "personal_force:intention") %>%
                    dplyr::pull(p.value)),
      `Eta squared` = purrr::map_dbl(data_long,
                              ~aov(rate ~ personal_force*intention,
                                   data=.x) %>%
                                effectsize::eta_squared() %>%
                                tibble::as_tibble() %>%
                                dplyr::filter(Parameter == "personal_force:intention") %>%
                                dplyr::pull(Eta2_partial) %>%
                                round(3)),
      `Raw effect` = purrr::map_dbl(data_long,
                             ~dplyr::group_by(.x, personal_force, intention) %>%
                               dplyr::summarise(avg_rate = mean(rate, na.rm = TRUE),
                                         .groups = "drop") %>%
                               tidyr::pivot_wider(names_from = c(personal_force, intention),
                                           values_from = avg_rate) %>%
                               dplyr::mutate(raw_diff = (`0_0`-`1_0`) - (`0_1` - `1_1`)) %>%
                               dplyr::pull(raw_diff) %>%
                               round(2))
    )
}

