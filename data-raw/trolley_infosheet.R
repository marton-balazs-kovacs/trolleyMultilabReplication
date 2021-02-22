library(tidyverse)
library(googlesheets4)

authors <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hacxT8-8Sd_ABBe_hyw3O4rc_2O7JgErlgW-o3SpdxM/edit?usp=sharing")

# names_sep <-
#   infosheet %>%
#   mutate(Firstname = str_remove(`Full Name as used for publishing`, " .*$"),
#          `Middle Name` = str_remove(`Full Name as used for publishing`, Firstname),
#          `Middle Name` = str_remove(`Middle Name`, `Last Name`)) %>%
#   select(`Full Name as used for publishing`, `Last Name`, Firstname, `Middle Name`)

core_team <- infosheet %>%
  filter(!is.na(`Order in publication`)) %>%
  mutate(`Core team` = 1L)

not_core_team <- infosheet %>%
  filter(is.na(`Order in publication`)) %>%
  arrange(Surname) %>%
  mutate(`Order in publication` = 7:245,
         `Core team` = 0L,
         Investigation = TRUE) %>%
  mutate_at(vars(11:14, 16:24), ~ FALSE)

infosheet_clean <- bind_rows(core_team, not_core_team)

infosheet_clean <-
  infosheet_clean %>%
  arrange(`Order in publication`)

usethis::use_data(trolley_infosheet, overwrite = TRUE)