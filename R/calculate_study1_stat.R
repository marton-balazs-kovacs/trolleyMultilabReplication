#' Calculates the statistics for study 1
#'
#' This is the description
#'
#' @param data data frame or tibble that contains Region and specified variables
#' @param vars the variables to be collapsed
#' @param label what data label should be attached to the output
#' @param rscale prior to be passed to the ttestBF function
#'
#' @return a tibble that contains the selected statistics
#'   in a format that can be printed
#' @export
calculate_study1_stat <- function(data = NULL,
                                  vars = NULL,
                                  label = NULL,
                                  rscale){

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
                             as.data.frame()),
           bttest = purrr::map(data_long,
                        ~BayesFactor::ttestBF(formula = rate ~ condition, data = .x,
                                 rscale = rscale, nullInterval = c(0, Inf)) %>%
                          tibble::as_tibble()),
           fttest = purrr::map(data_long,
                        ~t.test(rate ~ condition, data = .x) %>%
                          broom::tidy())) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Exclusion = label,
      Cluster = Region,
      BF = purrr::map_chr(bttest,
                   ~dplyr::slice(.x, 2) %>%
                     dplyr::pull(bf) %>%
                     scales::scientific()),
      RR = NA_character_,
      t = purrr::map_dbl(fttest,
                  ~dplyr::pull(.x, statistic) %>%
                    round(2)),
      df = purrr::map_dbl(fttest,
                   ~dplyr::pull(.x, parameter) %>%
                     round(2)),
      p = purrr::map_chr(fttest,
                  ~dplyr::pull(.x, p.value) %>%
                    scales::scientific()),
      `Cohen's d` = purrr::map_dbl(data_long,
                            ~effsize::cohen.d(rate ~ condition,
                                              data = .x)$estimate %>%
                              round(2)))


}
