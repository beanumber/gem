# Prepare data for 2018 GEM Women's report

library(tidyverse)
library(gem)

# Use new data
data(gem2020)
data(gem2021)
data("countries2021")

## variables

names(gem2021)

# setdiff(names(gem2020), names(gem_2019_supplement))

gem_womens_2022 <- gem2021 %>%
  rename(year = yrsurv) %>%
#  inner_join(select(countries, country, year = last_year), by = c("country", "year")) %>%
  fct_gem_2022()

usethis::use_data(gem_womens_2022, overwrite = TRUE)
