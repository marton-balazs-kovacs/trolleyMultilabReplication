#' Calculate the table stats for the additional analysis
#'
#' This is a super lazy function that needs to be generalized.
#'
#' @param data the piped reformatted data for the additional anaylsis
#' @export
calculate_additional_table <- function(data) {
  data %>%
    drop_na(rate, comparison) %>%
    nest(data = c(survey_id, task, rate)) %>%
    mutate(bf_res = map(data, ~ BayesFactor::ttestBF(formula = rate ~ task, data = .x, rscale = 0.26)),
           bf = map_dbl(bf_res, ~ round(my_bf_extractor(.x)$bf, 2)),
           freq_res = map(data, ~ t.test(rate ~ task, data = .x,  paired = FALSE) %>% broom::tidy()),
           t = map_dbl(freq_res, ~ round(.x$statistic, 2)),
           p = map_dbl(freq_res, ~ round(.x$p.value, 2)),
           df = map_dbl(freq_res, ~ round(.x$parameter, 2)))
}
