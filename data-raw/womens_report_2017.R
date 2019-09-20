# Prepare data for 2018 GEM Women's report

library(tidyverse)
library(gem)

load(file = "~/Data/gem/gem_2015-2018.rda")
map(gem, dim)
gem_1518 <- clean_gem(gem)

gem_1518 %>%
   group_by(year, country) %>%
   count(income_level) %>%
  print(n = Inf)

save(gem_1518, file = here::here("data", "gem_1518.rda"), compress = "xz")

# For countries missing from 2018, use 2017 data
missing_countries <- setdiff(unlist(countries$countries[3]),
                             unlist(countries$countries[4])) %>%
  sort()

gem_2018 <- gem_1518 %>%
  filter(year == 2018 | (year == 2017 & country %in% missing_countries))

# Use new data
load(file = "~/Data/gem/gem_2017-2018.rda")

countries <- gem_1718 %>%
  group_by(country) %>%
  summarize(N = n(), last_year = max(yrsurv))

gem_2018 <- gem_1718 %>%
  clean_gem() %>%
  rename(year = yrsurv) %>%
  inner_join(select(countries, country, year = last_year), by = c("country", "year")) %>%
  fct_gem()

save(gem_2018, file = here::here("data", "gem_2018.rda"), compress = "xz")
