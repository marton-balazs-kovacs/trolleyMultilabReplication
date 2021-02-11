# Add packages
usethis::use_package("dplyr")
usethis::use_package("kableExtra")
usethis::use_package("papaja")
usethis::use_package("lmerTest")
usethis::use_package("jsonlite")
usethis::use_package("vroom")
usethis::use_package("countrycode")
usethis::use_package("gt")
usethis::use_package("janitor")
usethis::use_package("here")
usethis::use_package("ggrepel")
usethis::use_package("purrr")
usethis::use_package("ggplot2")
usethis::use_package("tidyr")
usethis::use_package("furrr")
usethis::use_package("tibble")
usethis::use_package("msm")
usethis::use_package("rlang")
usethis::use_package("")
usethis::use_package("")
usethis::use_package("")

# Add pipe
usethis::use_pipe(export = TRUE)

# Add raw data files
usethis::use_data_raw("trolley_raw")
usethis::use_data_raw("trolley_preprocessed")
usethis::use_data_raw("trolley")
usethis::use_data_raw("cultural_distance")

# Add data files
usethis::use_data(correct_answers, qualtrics_surveys, internal = TRUE, overwrite = TRUE)

# Document package
devtools::document()

# Load package
devtools::load_all()

# Add R functions
usethis::use_r("aggregate_inferences")
usethis::use_r("draw_values")
usethis::use_r("generate_dataset")
usethis::use_r("get_lmbf_inference")
usethis::use_r("get_ttestbf_inference")
usethis::use_r("simulate")
usethis::use_r("add_ind_col_scale")
usethis::use_r("summarise_study")
usethis::use_r("")
usethis::use_r("")
usethis::use_r("")
usethis::use_r("")
usethis::use_r("")

# Add vignettes
usethis::use_vignette("supplementary_materials")
usethis::use_vignette("manuscript")
