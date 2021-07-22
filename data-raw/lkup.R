# Lookups

library(tidyverse)
library(here)

country_income_lkup <- read_csv(here("data-raw", "country_income.csv"))

save(country_income_lkup, file = here("data", "countries.rda"), compress = "xz")

countries2021 <- read_csv(here("data-raw", "countries_2021.csv"))

usethis::use_data(countries2021, overwrite = TRUE)
