# Prepare data for 2018 GEM Women's report

library(tidyverse)
library(gem)

# Use new data
load(file = "~/Data/gem/gem_2017-2018.rda")

gem_1718 %>%
  group_by(RPT2019) %>%
  count()

countries <- gem_1718 %>%
  group_by(country) %>%
  summarize(N = n(), last_year = max(yrsurv))

gem_2018 <- gem_1718 %>%
  filter(RPT2019 == "2019 sample") %>%
  clean_gem() %>%
  rename(year = yrsurv) %>%
#  inner_join(select(countries, country, year = last_year), by = c("country", "year")) %>%
  fct_gem()

gem_2018 %>%
  group_by(country) %>%
  summarize(N = n(), last_year = max(year))

save(gem_2018, file = here::here("data", "gem_2018.rda"), compress = "xz")
