# Prepare data for 2018 GEM Women's report

library(tidyverse)
library(gem)

# Use new data
data(gem2019)
data(gem2020)
data("countries2021")

countries_2020 <- gem2020 %>%
  group_by(country) %>%
  summarize(N = n(), last_year = max(yrsurv))

countries_2019 <- gem2019 %>%
  group_by(country) %>%
  summarize(N = n(), last_year = max(yrsurv))

countries_all <- countries_2020 %>%
  full_join(countries_2019, by = "country") %>%
  full_join(countries2021, by = c("country" = "country_alt"))

missing <- countries_2019 %>%
  anti_join(countries_2020, by = "country") %>%
  select(country)

gem_2019_supplement <- gem2019 %>%
  semi_join(missing)

gem_womens_2021 <- gem2020 %>%
  bind_rows(gem_2019_supplement)

gem_womens_2021 %>%
  group_by(country) %>%
  summarize(N = n(), last_year = max(yrsurv)) %>%
  print(n = Inf)


## variables

names(gem_2019_supplement)

names(gem2020)

setdiff(names(gem_2019_supplement), names(gem2020))
setdiff(names(gem2020), names(gem_2019_supplement))


gem_womens_2021 <- gem_womens_2021 %>%
  rename(year = yrsurv) %>%
#  inner_join(select(countries, country, year = last_year), by = c("country", "year")) %>%
  fct_gem_2021()

usethis::use_data(gem_womens_2021, overwrite = TRUE)
